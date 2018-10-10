#!/bin/sh
# See bbatsov/rubocop#3326
# rubocop:disable all
exec ruby "-S" "-x" "$0" "$@"
#! ruby
# rubocop:enable all

# The default prefix for the root temporary directory. See TempDir.root.
TMPDIR_PREFIX = "form_check_"

# The default maximal running time in seconds of FORM jobs before they get
# terminated.
TIMEOUT = 10

# The default directory for searching test cases.
TESTDIR = File.dirname(__FILE__)

# Check the Ruby version.
if RUBY_VERSION < "1.8.0"
  warn("ruby 1.8 required for the test suite")
  exit(1)
end

require "fileutils"
require "open3"
require "ostruct"
require "optparse"
require "set"
require "tmpdir"

# Show an error message and exit.
def fatal(message, file = nil, lineno = nil)
  if !file.nil? && !lineno.nil?
    STDERR.puts("#{file}:#{lineno}: error: #{message}")
  elsif !file.nil?
    STDERR.puts("#{file}: error: #{message}")
  else
    STDERR.puts("error: #{message}")
  end
  exit(1)
end

# Show a warning message.
def warn(message, file = nil, lineno = nil)
  if !file.nil? && !lineno.nil?
    STDERR.puts("#{file}:#{lineno}: warning: #{message}")
  elsif !file.nil?
    STDERR.puts("#{file}: warning: #{message}")
  else
    STDERR.puts("warning: #{message}")
  end
end

# Routines for temporary directories.
class TempDir
  @root = nil

  # Return the root temporary directory name.
  def self.root
    if @root.nil?
      @root = Dir.mktmpdir(TMPDIR_PREFIX)
    end
    @root
  end

  # Create a temporary directory under the root temporary directory, and return
  # the directory name.
  def self.mktmpdir(prefix)
    Dir.mktmpdir(prefix, root)
  end

  # Clean up the all temporary directory.
  def self.cleanup
    return if @root.nil?

    # The first try.
    FileUtils.rm_rf(@root)

    # Wait up to 5 seconds.
    50.times do
      # If the directory still remains, try to remove it after 0.1 seconds.
      if !FileTest.directory?(@root)
        return
      end

      sleep(0.1)
      FileUtils.rm_rf(@root)
    end

    # Failed.
    if FileTest.directory?(@root)
      warn("failed to delete the temporary directory '#{@root}'")
    end

    @root = nil
  end

  # We need to register the cleanup function before loading test/unit.
  at_exit { TempDir.cleanup }
end

# Register a finalization function before loading test/unit.
at_exit { defined?(finalize) && finalize }

# We use test/unit, which is now not in the standard library.
begin
  require "test/unit"
rescue LoadError
  warn("test/unit required for the test suite")
  exit(1)
end

# Find the path to a program.
def which(name)
  result = nil
  if name != File.basename(name)
    # Convert the relative path to the absolute path.
    result = File.expand_path(name)
  else
    # Search from $PATH.
    ENV["PATH"].split(":").each do |path|
      candidate = File.join(path, name)
      if File.executable?(candidate)
        result = File.expand_path(candidate)
        break
      end
    end
  end
  result = name if result.nil? # Fallback.
  result
end

# To be mixed-in all FORM tests.
module FormTest
  # Interplay with globals.

  @cfg = nil
  @tests = nil

  def self.cfg=(val)
    @cfg = val
  end

  def self.cfg
    @cfg
  end

  def self.tests=(val)
    @tests = val
  end

  def self.tests
    @tests
  end

  def info
    FormTest.tests.classes_info[self.class.name]
  end

  # Accessors to the configuration.

  def timeout
    FormTest.cfg.timeout
  end

  def ncpu
    FormTest.cfg.ncpu
  end

  def serial?
    FormTest.cfg.serial?
  end

  def threaded?
    FormTest.cfg.threaded?
  end

  def mpi?
    FormTest.cfg.mpi?
  end

  def valgrind?
    !FormTest.cfg.valgrind.nil?
  end

  def wordsize
    FormTest.cfg.wordsize
  end

  def cygwin?
    RUBY_PLATFORM =~ /cygwin/i
  end

  def mac?
    RUBY_PLATFORM =~ /darwin/i
  end

  def linux?
    RUBY_PLATFORM =~ /linux/i
  end

  def travis?
    ENV["TRAVIS"] == "true"
  end

  # Override methods in Test::Unit::TestCase.

  def setup
    super
    @tmpdir = nil
    @filename = nil
  end

  def teardown
    cleanup_files
    super
  end

  # Set up the working directory and put FORM files.
  def setup_files
    cleanup_files
    @tmpdir = TempDir.mktmpdir(self.class.name + "_")
    nfiles.times do |i|
      File.open(File.join(@tmpdir, "#{i + 1}.frm"), "w") do |file|
        file.write(info.sources[i])
      end
    end
  end

  # Delete the working directory.
  def cleanup_files
    if !@tmpdir.nil?
      FileUtils.rm_rf(@tmpdir)
    end
    @tmpdir = nil
  end

  # Called from derived classes' test_* methods.
  def do_test
    if !requires
      info.status = "SKIPPED"
      if defined?(omit)
        omit(requires_str) do
          yield
        end
      elsif defined?(skip)
        skip(requires_str)
      end
      return
    end

    if !FormTest.cfg.full && pendings
      info.status = "SKIPPED"
      if defined?(pend)
        pend(pendings_str) do
          assert(false)
          yield
        end
      elsif defined?(skip)
        skip(requires_str)
      end
      return
    end

    setup_files
    prepare
    @stdout = ""
    @stderr = ""
    begin
      nfiles.times do |i|
        @filename = "#{i + 1}.frm"
        execute("#{FormTest.cfg.form_cmd} #{@filename}")
        if !finished?
          info.status = "TIMEOUT"
          assert(false, "timeout (= #{timeout} sec) in #{@filename} of #{info.desc}")
        end
        if return_value != 0
          break
        end
      end
      yield
    # NOTE: Here we catch all exceptions, though it is a very bad style. This
    #       is because, in Ruby 1.9, test/unit is implemented based on
    #       minitest and MiniTest::Assertion is not a subclass of
    #       StandardError.
    rescue Exception => e # rubocop:disable Lint/RescueException
      STDERR.puts
      STDERR.puts("=" * 79)
      STDERR.puts("#{info.desc} FAILED")
      STDERR.puts("=" * 79)
      STDERR.puts(@stdout)
      STDERR.puts("=" * 79)
      STDERR.puts
      if info.status.nil?
        if (defined?(MiniTest::Assertion) && e.is_a?(MiniTest::Assertion)) ||
           (defined?(Test::Unit::AssertionFailedError) && e.is_a?(Test::Unit::AssertionFailedError))
          info.status = "FAILED"
        else
          info.status = "ERROR"
        end
      end
      raise e
    else
      if FormTest.cfg.verbose
        STDERR.puts
        STDERR.puts("=" * 79)
        STDERR.puts("#{info.desc} SUCCEEDED")
        STDERR.puts("=" * 79)
        STDERR.puts(@stdout)
        STDERR.puts("=" * 79)
        STDERR.puts
      end
      info.status = "OK"
    end
  end

  # Execute a FORM job.
  def execute(cmdline)
    @finished = false
    @exit_status = nil
    t0 = Time.now
    begin
      execute_popen3(cmdline, timeout)
    ensure
      t1 = Time.now
      dt = t1 - t0
      if info.times.nil?
        info.times = []
      end
      info.times.push(dt)
    end
  end

  # An implementation by popen3. Should work with Ruby 1.8 on Unix.
  #
  # tested on:
  #   ruby 1.8.5 (2006-08-25) [x86_64-linux]
  #   ruby 1.8.7 (2013-12-22 patchlevel 375) [x86_64-linux]
  #   ruby 1.9.3p484 (2013-11-22 revision 43786) [x86_64-linux]
  #   ruby 1.9.3p545 (2014-02-24) [i386-cygwin]
  #   ruby 1.9.3p545 (2014-02-24) [x86_64-cygwin]
  #   ruby 2.0.0p247 (2013-06-27) [x86_64-linux]
  #   ruby 2.0.0p481 (2014-05-08 revision 45883) [x86_64-linux]
  #   ruby 2.1.4p265 (2014-10-27 revision 48166) [x86_64-linux]
  #
  # segfault at IO#gets
  #   ruby 1.8.6 (2010-09-02 patchlevel 420) [x86_64-linux]
  #   ruby 1.8.7 (2012-02-08 patchlevel 358) [x86_64-linux]
  #
  def execute_popen3(cmdline, timeout)
    cmdline = "echo pid=$$;cd #{@tmpdir};#{cmdline};echo exit_status=$?"

    stdout = []
    stderr = []

    Open3.popen3(cmdline) do |stdinstream, stdoutstream, stderrstream|
      stdinstream.close
      out = Thread.new do
        while (line = stdoutstream.gets)
          stdout << line
        end
      end
      err = Thread.new do
        while (line = stderrstream.gets)
          stderr << line
          # We print both stdout and stderr when a test fails. An easy way to
          # implement this is to copy messages in stderr to those in stdout.
          # Unfortunately their orders are not preserved.
          stdout << line
        end
      end
      begin
        runner = Thread.current
        killer = Thread.new(timeout) do |timeout1|
          sleep(timeout1)
          runner.raise
        end
        out.join
        err.join
        killer.kill
      rescue StandardError
        while out.alive? && stdout.empty?
          sleep(0.01)
        end
        if !stdout.empty? && stdout[0] =~ /pid=([0-9]+)/
          pid = $1.to_i
          Process.kill("KILL", pid)
        else
          warn("failed to kill FORM job at timeout (unknown pid)")
        end
      else
        @finished = true
      ensure
        out.kill # avoid SEGFAULT at IO#close in some old versions
        err.kill
      end
    end

    if !stdout.empty? && stdout[0] =~ /pid=([0-9]+)/
      stdout.shift
    end

    if !FormTest.cfg.valgrind.nil?
      # The exit status may be in the middle of the output (sometimes annoyingly
      # happened on Travis CI).
      if @finished && !stdout.empty? && !stdout[-1].start_with?("exit_status=")
        i = stdout.map { |x| x.start_with?("exit_status") }.rindex(true)
        if !i.nil?
          s = stdout[i]
          stdout.delete_at(i)
          stdout << s
        end
      end
    end

    if @finished && !stdout.empty? && stdout[-1] =~ /exit_status=([0-9]+)/
      @exit_status = $1.to_i
      stdout.pop
    end

    # We exclude the Valgrind warnings "Warning: set address range perms: ..."
    # from the standard output, which can happen when the program allocates
    # big memory chunks.
    stdout = stdout.reject { |l| l =~ /Warning: set address range perms/ }

    @stdout += stdout.join
    @stderr += stderr.join
  end

  # Default assertions.
  def default_check
    if return_value != 0
      assert(false, "nonzero return value (= #{return_value}) from #{@filename} of #{info.desc}")
    elsif warning?
      assert(false, "warning in #{@filename} of #{info.desc}")
    else
      assert(true)
    end
  end

  # Methods to be overridden in derived classes.

  # The number of FORM files attached to the test.
  def nfiles
    1
  end

  # The required condition. The test will be skipped if the condition does not
  # hold.
  def requires
    true
  end

  # The string representation for the required condition.
  def requires_str
    "true"
  end

  # The pending condition. The test will be skipped if the condition holds.
  def pendings
    false
  end

  # The string representation for the pending condition.
  def pendings_str
    "false"
  end

  # The method to be called before the test.
  def prepare
    # Can be overridden in child classes.
  end

  # Test-result functions.

  # The exit status as a number
  def return_value
    @exit_status
  end

  # The verbatim result keeping line breaks and whitespaces.
  # Must be in the default output format.
  def exact_result(exprname, index = -1)
    matches = @stdout.scan(/^[ \t]+#{Regexp.escape(exprname)}\s*=(.+?);/m)
    return matches[index].first if !matches.empty? && !matches[index].nil?

    ""
  end

  # The result on one line with multiple whitespaces reduced to one.
  # Must be in the default output format.
  def result(exprname, index = -1)
    r = exact_result(exprname, index)
    return r.gsub(/\s+/, "") if !r.nil?

    ""
  end

  # The number of terms in the given expression.
  # Must be in the default statistics format.
  def nterms(exprname, index = -1)
    matches = @stdout.scan(/^[ \t]+#{exprname}\s*Terms in output\s*=\s*(\d+)\s*Bytes used\s*=\s*\d+/m)
    return matches[index].first.to_i if !matches.empty? && !matches[index].nil?

    -1
  end

  # The size in byte.
  # Must be in the default statistics format.
  def bytesize(exprname, index = -1)
    matches = @stdout.scan(/^[ \t]+#{exprname}\s*Terms in output\s*=\s*\d+\s*Bytes used\s*=\s*(\d+)/m)
    return matches[index].first.to_i if !matches.empty? && !matches[index].nil?

    -1
  end

  # The file contents as a string (in the working directory).
  def file(filename)
    begin
      File.open(File.join(@tmpdir, filename), "r") do |f|
        return f.read
      end
    rescue StandardError
      STDERR.puts("warning: failed to read '#{filename}'")
    end
    ""
  end

  # Same as file(filename).
  def read(filename)
    file filename
  end

  # Write to a file (in the working directory).
  def write(filename, text)
    fname = File.join(@tmpdir, filename)
    FileUtils.mkdir_p(File.dirname(fname))
    File.open(fname, "w") do |f|
      f.write(text)
    end
  end

  # The working directory for the test.
  def workdir
    @tmpdir
  end

  # The standard output of the FORM job as a string.
  def stdout
    @stdout
  end

  # The standard error of the FORM job as a string.
  def stderr
    @stderr
  end

  # Test-result functions to be used in assertions.

  # true if the FORM job finished in timeout.
  def finished?
    @finished
  end

  # true if the FORM job put warning messages.
  def warning?
    @stdout =~ /Warning/
  end

  # true if the FORM job put compile time errors.
  def compile_error?
    @stdout =~ /#{@filename} Line \d+ -->/
  end

  # true if the FORM job put run time errors.
  def runtime_error?
    if serial?
      @stdout =~ /Program terminating at #{@filename} Line \d+ -->/
    elsif threaded?
      @stdout =~ /Program terminating in thread \d+ at #{@filename} Line \d+ -->/
    elsif mpi?
      @stdout =~ /Program terminating in process \d+ at #{@filename} Line \d+ -->/
    end
  end

  # true if the FORM job completed without any warnings/errors and
  # the exit code was 0.
  def succeeded?
    if finished? && !warning? && !compile_error? && !runtime_error? && return_value == 0
      if FormTest.cfg.valgrind.nil?
        if @stderr.empty?
          return true
        end

        @stdout += "!!! stderr is not empty"
        return false
      end
      # Check for Valgrind errors.
      ok = !@stderr.include?("Invalid read") &&
           !@stderr.include?("Invalid write") &&
           !@stderr.include?("Invalid free") &&
           !@stderr.include?("Mismatched free") &&
           !@stderr.include?("Use of uninitialised value") &&
           !@stderr.include?("Conditional jump or move depends on uninitialised value") &&
           !@stderr.include?("points to unaddressable byte") &&
           !@stderr.include?("points to uninitialised byte") &&
           !@stderr.include?("contains uninitialised byte") &&
           !@stderr.include?("Source and destination overlap in memcpy") &&
           !@stderr.include?("has a fishy") &&
           @stderr !~ /definitely lost: [1-9]/ &&
           @stderr !~ /indirectly lost: [1-9]/ &&
           @stderr !~ /possibly lost: [1-9]/
      if !ok
        @stdout += "!!! Valgrind test failed"
      end
      return ok
    end
    false
  end

  # Utility functions for pattern matching.

  # A pattern from the given string with escaping any special characters.
  def exact_pattern(str)
    san_str = Regexp.quote(str)
    Regexp.new(san_str)
  end

  # The same as #exact_pattern but ignores whitespaces.
  def pattern(str)
    san_str = Regexp.quote(str.gsub(/\s+/, ""))
    Regexp.new(san_str)
  end

  # Same as #pattern but matches only with the whole expression.
  # Assumes the default output format.
  def expr(str)
    san_str = Regexp.quote(str.gsub(/\s+/, ""))
    Regexp.new("^" + san_str + "$")
  end
end

# Information of a test case.
class TestInfo
  def initialize
    @where = nil    # where the test is defined
    @foldname = nil # fold name of the test
    @enabled = nil  # enabled or not
    @sources = []   # FORM sources

    @status = nil   # status
    @times = nil    # elapsed time (array)
  end

  attr_accessor :where, :foldname, :enabled, :sources, :status, :times

  # Return the description of the test.
  def desc
    "#{@foldname} (#{@where})"
  end
end

# List of test cases.
class TestCases
  def initialize
    @files = []             # Ruby files

    @classes = []           # test class names (unsorted)
    @classes_set = Set.new  # set of test class names
    @classes_info = {}      # TestInfo objects, key: Ruby class name

    @name_patterns = []
    @exclude_patterns = []
  end

  attr_reader :classes_info
  attr_accessor :name_patterns, :exclude_patterns

  # Return a list containing info objects for enabled tests.
  def classes_info_list
    infos = []
    @classes.each do |c|
      info = @classes_info["Test_" + c]
      if info.enabled
        infos.push(info)
      end
    end
    infos
  end

  # Convert a .frm file to a .rb file and load it.
  def make_ruby_file(filename)
    # Check existing files.
    inname = File.basename(filename)
    outname = File.basename(filename, ".frm") + ".rb"
    if @files.include?(outname)
      fatal("duplicate output file name", inname)
    end
    @files.push(outname)

    outname = File.join(TempDir.root, outname)

    File.open(filename, "r") do |infile|
      File.open(outname, "w") do |outfile|
        lineno = 0
        level = 0
        classname = nil
        info = nil
        block = nil
        blockno = 0
        fileno = 0
        skipping = false
        heredoc = nil
        requires = nil
        pendings = nil
        prepares = nil

        infile.each_line do |line|
          line.chop!
          lineno += 1
          if level == 0
            if line =~ /^\*..#\[\s*([^:]*)/
              # fold open: start a class
              fold = $1.strip
              if fold.empty?
                fatal("empty fold", inname, lineno)
              end
              classname = canonical_name(fold)
              info = TestInfo.new
              @classes.push(classname)
              @classes_set.add(classname)
              @classes_info["Test_#{classname}"] = info
              info.where = "#{inname}:#{lineno}"
              info.foldname = fold
              info.enabled = test_enabled?(classname)

              level += 1
              block = ""
              blockno = 0
              fileno = 0
              skipping = !info.enabled
              heredoc = nil
              requires = nil
              pendings = nil
              prepares = nil
              if skipping
                line = ""
              else
                line = "class Test_#{classname} < Test::Unit::TestCase; include FormTest"
              end
            elsif line =~ /^\*..#\]/
              # unexpected fold close
              fatal("unexpected fold close", inname, lineno)
            else
              # as commentary
              line = ""
            end
          elsif heredoc.nil? && line =~ /^\*..#\]\s*([^:]*)/ && level == 1
            # fold close: end of the class
            fold = $1.strip
            foldname = info.foldname
            if !fold.empty? && fold != foldname
              warn("unmatched fold '#{fold}', which should be '#{foldname}'", inname, lineno)
            end

            line = ""
            if !skipping
              if fileno == 0
                # no .end
                blockno.times do
                  outfile.write("\n")
                end

                block += ".end\n"
                fileno += 1
                info.sources.push(block)

                line += "def test_#{classname}; do_test { default_check } end; "
              else
                outfile.write("def test_#{classname}; do_test {\n" + block)
                line = "} end; "
              end
              line += "def nfiles; #{fileno} end; " if fileno != 1
              if !requires.nil?
                requires = requires.map { |s| "(" + s + ")" }.join(" && ")
                line += "def requires; #{requires} end; "
                line += "def requires_str; %(#{requires}) end; "
              end
              if !pendings.nil?
                pendings = pendings.map { |s| "(" + s + ")" }.join(" || ")
                line += "def pendings; #{pendings} end; "
                line += "def pendings_str; %(#{pendings}) end; "
              end
              if !prepares.nil?
                prepares = prepares.join("; ")
                line += "def prepare; #{prepares} end; "
              end
              line += "end"
            end
            level = 0
            classname = nil
            info = nil
          elsif heredoc.nil? && line =~ /^\s*\.end/
            # .end
            if skipping
              line = ""
            else
              blockno += 1 if fileno > 0 # previous .end
              blockno.times do
                outfile.write("\n")
              end

              block += line + "\n"
              fileno += 1
              info.sources.push(block)

              block = ""
              blockno = 0

              line = nil # later
            end
          elsif heredoc.nil? && line =~ /^\s*#\s*require\s+(.*)/
            # #require <condition>
            line = ""
            if requires.nil?
              requires = []
            end
            requires << $1
          elsif heredoc.nil? && line =~ /^\s*#\s*pend_if\s+(.*)/
            # #pend_if <condition>
            line = ""
            if pendings.nil?
              pendings = []
            end
            pendings << $1
          elsif heredoc.nil? && line =~ /^\s*#\s*prepare\s+(.*)/
            # #prepare <statement>
            line = ""
            if prepares.nil?
              prepares = []
            end
            prepares << $1
          elsif heredoc.nil? && line =~ /^\*\s*#\s*(require|prepare|pend_if)\s+(.*)/
            # *#require/prepare/pend_if, commented out in the FORM way
            line = ""
          else
            if heredoc.nil?
              if line =~ /^\*..#\[/
                # fold open
                level += 1
              elsif line =~ /^\*..#\]\s*([^:]*)/
                # fold close
                level -= 1
              elsif line =~ /<</ && (line =~ /<<-?(\w+)/ ||
                                     line =~ /<<-?"(\w+)"/ ||
                                     line =~ /<<-?'(\w+)'/ ||
                                     line =~ /<<-?`(\w+)`/)
                # start here document
                heredoc = Regexp.new($1)
                # NOTE: Currently, we don't support more than one << operators
                #       in the same line.
              end
            elsif line =~ heredoc
              # end here document
              heredoc = nil
            end
            if skipping
              line = ""
            else
              # some typical assertions
              if line =~ /^\s*assert\s+(succeeded\?|finished\?)\s*$/
                line = "assert(#{$1}, 'Failed for #{$1}')"
              end
              block += line + "\n"
              blockno += 1
              line = nil
            end
          end
          if !line.nil?
            outfile.write(line + "\n")
          end
        end
        if level >= 1
          fatal("expected fold close", inname, lineno)
        end
      end
    end
    require outname
  end

  # true if the test is enabled
  def test_enabled?(name)
    # construct regular expressions (wildcards: '*' and '?')
    @name_patterns.length.times do |i|
      if !@name_patterns[i].is_a?(Regexp)
        s = @name_patterns[i].to_s.gsub("\*", ".*").tr("\?", ".")
        s = "^" + s + "$"
        @name_patterns[i] = Regexp.new(s)
      end
    end
    @exclude_patterns.length.times do |i|
      if !@exclude_patterns[i].is_a?(Regexp)
        s = @exclude_patterns[i].to_s.gsub("\*", ".*").tr("\?", ".")
        s = "^" + s + "$"
        @exclude_patterns[i] = Regexp.new(s)
      end
    end
    # check --name NAME
    ok = true
    if !@name_patterns.empty?
      ok = false
      @name_patterns.each do |pat|
        if name =~ pat
          ok = true
          break
        end
      end
    end
    if !ok
      return false
    end

    # check --execlude NAME
    if !@exclude_patterns.empty?
      @exclude_patterns.each do |pat|
        if name =~ pat
          ok = false
          break
        end
      end
    end
    ok
  end

  # Return a class name that is valid and unique.
  def canonical_name(name)
    prefix = name.gsub(/[^a-zA-Z0-9_]/, "_")
    s = prefix
    i = 0
    loop do
      if !@classes.include?(s)
        break
      end

      i += 1
      s = prefix + "_" + i.to_s
    end
    s
  end
end

# FORM configuration.
class FormConfig
  def initialize(form, mpirun, valgrind, ncpu, timeout, stat, full, verbose)
    @form     = form
    @mpirun   = mpirun
    @valgrind = valgrind
    @ncpu     = ncpu
    @timeout  = timeout
    @stat     = stat
    @full     = full
    @verbose  = verbose

    @form_bin      = nil
    @mpirun_bin    = nil
    @valgrind_bin  = nil
    @valgrind_supp = nil

    @head        = nil
    @is_serial   = nil
    @is_threaded = nil
    @is_mpi      = nil
    @wordsize    = nil
    @form_cmd    = nil
  end

  attr_reader :form, :mpirun, :valgrind, :ncpu, :timeout, :stat, :full, :verbose
  attr_reader :form_bin, :mpirun_bin, :valgrind_bin, :valgrind_supp
  attr_reader :head, :wordsize, :form_cmd

  def serial?
    @is_serial
  end

  def threaded?
    @is_threaded
  end

  def mpi?
    @is_mpi
  end

  def check_bin(name, bin)
    # Check if the executable is available.
    system("cd #{TempDir.root}; type #{bin} >/dev/null 2>&1")
    if $? == 0
      # OK.
      return
    end

    if name == bin
      fatal("executable '#{name}' not found")
    else
      fatal("executable '#{name}' ('#{bin}') not found")
    end
  end

  def check
    # Check if FORM is available.
    @form_bin = which(@form)
    check_bin(@form, @form_bin)
    # Check if Valgrind is available.
    if !@valgrind.nil?
      @valgrind_bin = which(@valgrind)
      check_bin(@valgrind, @valgrind_bin)
    end
    # Check the FORM version.
    tmpdir = TempDir.mktmpdir("ver_")
    begin
      frmname = File.join(tmpdir, "ver.frm")
      File.open(frmname, "w") do |f|
        f.write(<<-'TEST_FRM')
  #-
  Off finalstats;
  .end
        TEST_FRM
      end
      @head = `#{@form_bin} #{frmname} 2>/dev/null`.split("\n").first
      @is_serial = false
      if @head =~ /^FORM/
        @is_serial   = true
        @is_threaded = false
        @is_mpi      = false
      elsif @head =~ /^TFORM/
        @is_serial   = false
        @is_threaded = true
        @is_mpi      = false
      elsif @head =~ /^ParFORM/
        @is_serial   = false
        @is_threaded = false
        @is_mpi      = true
      else
        system("#{form_bin} #{frmname}")
        fatal("failed to get the version of '#{@form}'")
      end
      if @head =~ /FORM[^(]*\([^)]*\)\s*(\d+)-bits/
        @wordsize = $1.to_i / 16
      else
        system("#{form_bin} #{frmname}")
        fatal("failed to get the wordsize of '#{@form}'")
      end
      # Prepare for mpirun
      if @is_mpi
        @mpirun_bin = which(@mpirun)
        check_bin(@mpirun, @mpirun_bin)
        # Open MPI is known to be not Valgrind-clean. Try to suppress some
        # errors. Unfortunately, it would be insufficient.
        supp = File.expand_path(File.join(File.dirname(@mpirun_bin),
                                          "..", "share", "openmpi",
                                          "openmpi-valgrind.supp"))
        if File.exist?(supp)
          @valgrind_supp = supp
        end
      end
      # Construct the command.
      cmdlist = []
      if @is_mpi
        cmdlist << @mpirun_bin << "-np" << @ncpu.to_s
      end
      if !@valgrind_bin.nil?
        cmdlist << @valgrind_bin
        cmdlist << "--leak-check=full"
        if !@valgrind_supp.nil?
          cmdlist << "--suppressions=#{@valgrind_supp}"
        end
      end
      cmdlist << @form_bin
      if @is_threaded
        cmdlist << "-w#{@ncpu}"
      end
      @form_cmd = cmdlist.join(" ")
      # Check the output header.
      @head = `#{@form_cmd} #{frmname} 2>/dev/null`.split("\n").first
      if $? != 0
        system("#{form_cmd} #{frmname}")
        fatal("failed to execute '#{@form_cmd}'")
      end
      if !@valgrind.nil?
        # Include valgrind version information.
        @head += "\n" + `#{@form_cmd} @{frmname} 2>&1 >/dev/null | grep Valgrind`.split("\n")[0]
      end
    ensure
      FileUtils.rm_rf(tmpdir)
    end
  end
end

# Return paths obtained by `oldpath` + `newpath`.
def add_path(oldpath, newpath)
  newpath = File.expand_path(newpath)
  if oldpath.nil?
    return newpath
  end

  newpath + ":" + oldpath
end

# Parse `TEST=...`.
def parse_def(pat)
  if pat =~ /^TEST=(.*)/
    return $1
  end

  nil
end

# Search for the `file`.
def search_file(file, opts)
  f = file
  return f if File.exist?(f)

  if !opts.dir.nil?
    f = File.join(opts.dir, file)
    return f if File.exist?(f)
  end
  if !TESTDIR.nil?
    f = File.join(TESTDIR, file)
    return f if File.exist?(f)
  end
  fatal("file '#{file}' not found")
end

# Search for the `dir`.
def search_dir(dir, opts)
  d = dir
  return d if File.directory?(d)

  if !opts.dir.nil?
    d = File.join(opts.dir, dir)
    return d if File.directory?(d)
  end
  if !TESTDIR.nil?
    d = File.join(TESTDIR, dir)
    return d if File.directory?(d)
  end
  fatal("directory '#{dir}' not found")
end

def main
  # Parse options.

  opts = OpenStruct.new
  opts.list = false
  opts.path = nil
  opts.form = "form"
  opts.mpirun = "mpirun"
  opts.ncpu = 4
  opts.timeout = nil
  opts.stat = false
  opts.full = false
  opts.enable_valgrind = false
  opts.valgrind = "valgrind"
  opts.dir = nil
  opts.name_patterns = []
  opts.exclude_patterns = []
  opts.files = []
  opts.verbose = false

  parser = OptionParser.new
  parser.banner = "Usage: #{File.basename($0)} [options] [--] [binname] [files|tests..]"
  parser.on("-h", "--help",          "Show this help and exit")           { puts(parser); exit }
  parser.on("-l", "--list",          "List all tests and exit")           { opts.list = true }
  parser.on("--path PATH",           "Use PATH for executables")          { |path| opts.path = add_path(opts.path, path) }
  parser.on("--form BIN",            "Use BIN as FORM executable")        { |bin|  opts.form = bin }
  parser.on("--mpirun BIN",          "Use BIN as mpirun executable")      { |bin|  opts.mpirun = bin }
  parser.on("-w", "--ncpu N",        "Use N CPUs")                        { |n|    opts.ncpu = n.to_i }
  parser.on("-t", "--timeout N",     "Timeout N in seconds")              { |n|    opts.timeout = n.to_i }
  parser.on("--stat",                "Print detailed statistics")         { opts.stat = true }
  parser.on("--full",                "Full test, ignoring pending")       { opts.full = true }
  parser.on("--enable-valgrind",     "Enable Valgrind")                   { opts.enable_valgrind = true }
  parser.on("--valgrind BIN",        "Use BIN as Valgrind executable")    { |bin| opts.enable_valgrind = true; opts.valgrind = bin }
  parser.on("-C", "--directory DIR", "Directory for test cases")          { |dir| opts.dir = search_dir(dir, opts) }
  parser.on("-n", "--name NAME",     "Run tests matching NAME")           { |pat| opts.name_patterns << pat }
  parser.on("-x", "--exclude NAME",  "Do not run tests matching NAME")    { |pat| opts.exclude_patterns << pat }
  parser.on("-v", "--verbose",       "Do not suppress the test output")   { opts.verbose = true }
  parser.on("-D TEST=NAME",          "Alternative way to run tests NAME") { |pat| opts.name_patterns << parse_def(pat) }
  begin
    parser.parse!(ARGV)
  rescue OptionParser::ParseError => e
    STDERR.puts(e.backtrace.first + ": #{e.message} (#{e.class})")
    e.backtrace[1..-1].each { |m| STDERR.puts("\tfrom #{m}") }
    puts(parser)
    exit(1)
  end

  # Parse other arguments.

  while !ARGV.empty?
    if ARGV[0] =~ /\.frm$/
      opts.files << search_file(ARGV[0], opts)
    elsif ARGV[0] =~ /valgrind/
      opts.enable_valgrind = true
      opts.valgrind = ARGV[0]
    elsif ARGV[0] =~ /mpirun/ || ARGV[0] =~ /mpiexec/
      opts.mpirun = ARGV[0]
    elsif ARGV[0] =~ /form/ || ARGV[0] =~ /vorm/ || File.executable?(ARGV[0])
      opts.form = ARGV[0]
    elsif File.exist?(ARGV[0])
      opts.files << ARGV[0]
    else
      opts.name_patterns << ARGV[0]
    end
    ARGV.shift
  end

  # Make test cases.

  FormTest.tests = TestCases.new
  FormTest.tests.name_patterns = opts.name_patterns
  FormTest.tests.exclude_patterns = opts.exclude_patterns

  if opts.files.empty?
    Dir.glob(File.join(opts.dir.nil? ? TESTDIR : opts.dir, "*.frm")).sort.each do |file|
      opts.files << search_file(file, opts)
    end
  end

  opts.files.uniq.sort.each do |file|
    FormTest.tests.make_ruby_file(file)
  end

  # --list option.
  if opts.list
    infos = FormTest.tests.classes_info_list
    infos.each do |info|
      puts("#{info.foldname} (#{info.where})")
    end
    puts("#{infos.length} tests")
    exit
  end

  # --path option.
  if !opts.path.nil?
    ENV["PATH"] = opts.path + ":" + ENV["PATH"]
  end

  # Set FORMPATH
  ENV["FORMPATH"] = File.expand_path(opts.dir.nil? ? TESTDIR : opts.dir) +
                    (ENV["FORMPATH"].nil? ? "" : ":" + ENV["FORMPATH"])

  # Default timeout.
  if opts.timeout.nil?
    opts.timeout = TIMEOUT
    # Running Valgrind can be really slow.
    if opts.enable_valgrind
      opts.timeout *= 30
    end
  end

  # Initialize the FORM configuration.
  FormTest.cfg = FormConfig.new(opts.form,
                                opts.mpirun,
                                opts.enable_valgrind ? opts.valgrind : nil,
                                opts.ncpu,
                                opts.timeout > 1 ? opts.timeout : 1,
                                opts.stat,
                                opts.full,
                                opts.verbose)
  FormTest.cfg.check
  puts("Check #{FormTest.cfg.form_bin}")
  puts(FormTest.cfg.head)
end

def finalize
  return if FormTest.cfg.nil? || !FormTest.cfg.stat

  infos = FormTest.tests.classes_info_list

  return if infos.empty?

  # Print detailed statistics.

  term_width = guess_term_width

  max_foldname_width = infos.map { |info| info.foldname.length }.max
  max_where_width = infos.map { |info| info.where.length }.max + 2
  status_width = 7
  time_width = 13
  bar_width = term_width - max_foldname_width - max_where_width - status_width -
              time_width - 5

  if bar_width < 12
    bar_width = 12
  elsif bar_width > 40
    bar_width = 40
  end

  puts("timeout: #{FormTest.cfg.timeout}s")

  infos.each do |info|
    (0..info.sources.length - 1).each do |i|
      t = 0
      if !info.times.nil? && i < info.times.length
        t = info.times[i]
      end
      if i == 0
        puts(format("%s %s  %s %s%s",
                    lpad(info.foldname, max_foldname_width),
                    lpad("(" + info.where + ")", max_where_width),
                    lpad(info.status.nil? ? "UNKNOWN" : info.status, status_width),
                    bar_str(t, FormTest.cfg.timeout, bar_width),
                    format_time(t, FormTest.cfg.timeout)))
      else
        puts(format("%s %s  %s %s%s",
                    lpad("", max_foldname_width),
                    lpad("", max_where_width),
                    lpad("", status_width),
                    bar_str(t, FormTest.cfg.timeout, bar_width),
                    format_time(t, FormTest.cfg.timeout)))
      end
    end
  end
end

# Return the string with padding to left.
def lpad(str, len)
  if str.length > len
    str[0..len - 1]
  elsif str.length < len
    str + " " * (len - str.length)
  else
    str
  end
end

# Return a string for a bar chart.
def bar_str(value, max_value, bar_width)
  bar_body_width = bar_width - 2
  bar = " " * bar_width
  bar[0] = "|"
  bar[bar_width - 1] = "|"
  pos = (Float(value) / max_value * bar_body_width).round
  if pos < 0
    pos = 0
  elsif pos > bar_body_width
    pos = bar_body_width
  end
  if pos >= 1
    (1..pos).each do |i|
      bar[i] = "#"
    end
  end
  bar
end

# Format an elapsed time.
def format_time(time, max_time)
  overflow = time > max_time
  if overflow
    t = max_time
  else
    t = time
  end
  t = Float(t)
  h = Integer(t / 3600)
  t = t % 3600
  m = Integer(t / 60)
  t = t % 60
  s = Integer(t)
  t = t % 1
  ms = Integer(t * 1000)
  format("%s%02d:%02d:%02d.%03d", overflow ? ">" : " ", h, m, s, ms)
end

# Return a guessed terminal width.
def guess_term_width
  require "io/console"
  IO.console.winsize[1]
rescue LoadError, NoMethodError
  system("type tput >/dev/null 2>&1")
  if $? == 0
    cols = `tput cols`
  else
    cols = ENV["COLUMNS"] || ENV["TERM_WIDTH"]
  end
  begin
    Integer(cols)
  rescue ArgumentError, TypeError
    80
  end
end

if $0 == __FILE__
  main
end
