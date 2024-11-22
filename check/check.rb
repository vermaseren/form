#!/bin/sh
# frozen_string_literal: true

# See bbatsov/rubocop#3326
# rubocop:disable all
exec ruby "-S" "-x" "$0" "$@"
#! ruby
# rubocop:enable all

# Check the Ruby version.
if RUBY_VERSION < "2.0.0"
  warn("ruby 2.0 required for the test suite")
  exit(1)
end

require "fileutils"
require "io/console/size"
require "open3"
require "optparse"
require "ostruct"
require "rbconfig"
require "set"
require "thread"
require "tmpdir"

# Show an error message and exit.
def fatal(message, file = nil, lineno = nil)
  if !file.nil? && !lineno.nil?
    $stderr.puts("#{file}:#{lineno}: error: #{message}")
  elsif !file.nil?
    $stderr.puts("#{file}: error: #{message}")
  else
    $stderr.puts("error: #{message}")
  end
  exit(1)
end

# Show a warning message.
def warn(message, file = nil, lineno = nil)
  if !file.nil? && !lineno.nil?
    $stderr.puts("#{file}:#{lineno}: warning: #{message}")
  elsif !file.nil?
    $stderr.puts("#{file}: warning: #{message}")
  else
    $stderr.puts("warning: #{message}")
  end
end

# Get a string from the given environment variable.
def read_env_str(key, default_value)
  value = default_value
  if ENV.key?(key)
    value = ENV[key]
  end
  value
end

# Get a positive integer from the given environment variable.
def read_env_positive_int(key, default_value)
  if default_value <= 0
    raise ArgumentError, "invalid default_value: #{default_value}"
  end

  value = default_value
  if ENV.key?(key)
    begin
      value = Integer(ENV[key])
      raise ArgumentError if value <= 0
    rescue ArgumentError, TypeError
      warn("environment variable ignored: #{key} is not positive integer: #{ENV[key]}")
      value = default_value
    end
  end
  value
end

# Get the total size of the physical memory available on the host machine.
def total_physical_memory
  platform = RbConfig::CONFIG["host_os"].downcase
  if platform.include?("linux")
    mem_info = `free -b | grep Mem`
    mem_info.split[1].to_i
  elsif platform.include?("darwin")
    mem_info = `sysctl -n hw.memsize`
    mem_info.to_i
  elsif platform.include?("mingw") || platform.include?("mswin")
    mem_info = `wmic ComputerSystem get TotalPhysicalMemory`
    mem_info.split[1].to_i
  end
end

# The default prefix for the root temporary directory. See TempDir.root.
TMPDIR_PREFIX = "form_check_"

# The default extra options for mpirun.
DEFAULT_MPIRUN_OPTS = read_env_str("FORM_CHECK_MPIRUN_OPTS", nil)

# The default extra options for valgrind.
DEFAULT_VALGRIND_OPTS = read_env_str("FORM_CHECK_VALGRIND_OPTS", nil)

# The default maximal running time in seconds of FORM jobs before they get terminated.
DEFAULT_TIMEOUT = read_env_positive_int("FORM_CHECK_DEFAULT_TIMEOUT", 10)

# The factor multiplied to DEFAULT_TIMEOUT when Valgrind is used.
VALGRIND_TIME_DILATION = read_env_positive_int("FORM_CHECK_VALGRIND_TIME_DILATION", 30)

# The default directory for searching test cases.
TESTDIR = File.dirname(__FILE__)

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

# Register a function before loading test/unit.
at_exit { defined?(output_detailed_statistics) && output_detailed_statistics }

# We use test/unit, which is now not in the standard library.
begin
  require "test/unit"
rescue LoadError
  warn("test/unit required for the test suite")
  exit(1)
end

# Try a monkey patch to call output_detailed_statistics() before the test suite summary.
begin
  require "test/unit/ui/console/testrunner"

  module Test
    module Unit
      module UI
        module Console
          class TestRunner
            alias old_output_statistics output_statistics
            alias _old_output output
            alias _old_output_summary_marker output_summary_marker

            def output_statistics(*args, **kwargs, &block)
              if need_output_detailed_statistics?
                output_summary_marker
                output_detailed_statistics(method(:output))
                output_summary_marker
              end
              old_output_statistics(*args, **kwargs, &block)
            end
          end
        end
      end
    end
  end
rescue NameError, LoadError
  # do nothing
end

# Find the path to a program.
def which(name)
  result = nil
  if name != File.basename(name)
    # Convert the relative path to the absolute path.
    result = File.expand_path(name)
  else
    # Search from $PATH.
    ENV["PATH"].split(File::PATH_SEPARATOR).each do |path|
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

  # Host environment.

  def cygwin?
    RbConfig::CONFIG["host_os"] =~ /cygwin/i
  end

  def mac?
    RbConfig::CONFIG["host_os"] =~ /darwin|mac os/i
  end

  def linux?
    RbConfig::CONFIG["host_os"] =~ /linux/i
  end

  def unix?
    cygwin? || mac? || linux? || RbConfig::CONFIG["host_os"] =~ /solaris|bsd/i
  end

  def windows?
    # NOTE: "cygwin" is intentionally excluded.
    RbConfig::CONFIG["host_os"] =~ /mswin|msys|mingw|bccwin|wince|emc/i
  end

  def travis?
    ENV["TRAVIS"] == "true"
  end

  def github?
    ENV["GITHUB_ACTIONS"] == "true"
  end

  # Total memory in bytes. -1 if undetermined.
  def total_memory
    @@total_memory_mutex.synchronize do
      if @@cached_total_memory.nil?
        result = total_physical_memory
        if result.nil?
          @@cached_total_memory = -1
        else
          @@cached_total_memory = result
        end
      end
      @@cached_total_memory
    end
  end

  @@cached_total_memory = nil
  @@total_memory_mutex = Mutex.new

  def reveal_newlines(str)
    if FormTest.cfg.show_newlines
      str.gsub("\r", "<CR>").gsub("\n", "<LF>\n")
    else
      str
    end
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
    @tmpdir = TempDir.mktmpdir("#{self.class.name}_")
    nfiles.times do |i|
      File.write(File.join(@tmpdir, "#{i + 1}.frm"), info.sources[i])
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
  def do_test(&block)
    if !requires
      info.status = "SKIPPED"
      if defined?(omit)
        omit(requires_str, &block)
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

    FormTest.cfg.retries.times do |t|
      setup_files
      prepare
      @stdout = ""
      @stderr = ""
      begin
        nfiles.times do |i|
          ENV["FORM"] = FormTest.cfg.form_cmd
          @filename = "#{i + 1}.frm"
          execute("#{ulimits}#{FormTest.cfg.form_cmd} #{@filename}")
          if !finished?
            info.status = "TIMEOUT"
            assert(false, "timeout (= #{timeout} sec) in #{@filename} of #{info.desc}")
          end
          if return_value != 0
            break
          end
        end
        # On Windows, we convert newline characters in stdout/stderr into
        # the Unix-style newline characters used in our test cases.
        @raw_stdout = @stdout
        @raw_stderr = @stderr
        if windows?
          @stdout = @stdout.gsub("\r\n", "\n")
          @stderr = @stderr.gsub("\r\n", "\n")
        end
        # MesPrint inevitably inserts newline characters when a line exceeds
        # its length limit. To verify error/warning messages, here we remove
        # newline characters that seem to be part of continuation lines
        # (this simple implementation also removes newline characters that are
        # not due to MesPrint).
        @cleaned_stdout = @stdout.gsub(/\R(?= ?\S)(?!\S+ Line \d+)/, "")

        yield
      # NOTE: Here we catch all exceptions, though it is a very bad style. This
      #       is because, in Ruby 1.9, test/unit is implemented based on
      #       minitest and MiniTest::Assertion is not a subclass of
      #       StandardError.
      rescue Exception => e # rubocop:disable Lint/RescueException
        if info.status == "TIMEOUT" && t < FormTest.cfg.retries - 1
          warn("timeout (= #{timeout} sec) in #{@filename} of #{info.desc}, retry")
          info.status = nil
          info.times = nil
          next
        end
        $stderr.puts
        $stderr.puts("=" * 79)
        $stderr.puts("#{info.desc} FAILED")
        $stderr.puts("=" * 79)
        $stderr.puts(reveal_newlines(@raw_stdout))
        $stderr.puts("=" * 79)
        $stderr.puts
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
          $stderr.puts
          $stderr.puts("=" * 79)
          $stderr.puts("#{info.desc} SUCCEEDED")
          $stderr.puts("=" * 79)
          $stderr.puts(reveal_newlines(@raw_stdout))
          $stderr.puts("=" * 79)
          $stderr.puts
        end
        info.status = "OK"
      end
      break
    end
  end

  # Execute a FORM job.
  def execute(cmdline)
    if FormTest.cfg.verbose
      $stderr.puts("Command: #{cmdline}")
    end
    @finished = false
    @exit_status = nil
    t0 = Time.now
    begin
      result = execute_impl(cmdline, timeout, @tmpdir)
      @finished = result[:finished_in_time]
      @exit_status = result[:exit_status]
      # We print both stdout and stderr when a test fails. An easy way to
      # implement this is to copy messages in stderr to those in stdout,
      # namely, use the combined output. Unfortunately their orders
      # (how stdout and stderr are merged) may not be preserved.
      # Note that we exclude the Valgrind warnings "Warning: set address range perms: ..."
      # which can happen when the program allocates big memory chunks.
      out = result[:combined_lines]
      out = out.reject { |l| l =~ /Warning: set address range perms/ }
      @stdout += out.join
      @stderr += result[:stderr_lines].join
    ensure
      t1 = Time.now
      dt = t1 - t0
      if info.times.nil?
        info.times = []
      end
      info.times.push(dt)
    end
  end

  def execute_impl(cmd, timeout, chdir)
    stdout_lines = []
    stderr_lines = []
    combined_lines = []
    exit_status = nil
    finished_in_time = false

    mutex = Mutex.new

    Open3.popen3(cmd, chdir: chdir) do |stdin, stdout, stderr, wait_thr|
      stdin.close

      stdout_thread = Thread.new do
        while (line = stdout.gets)
          stdout_lines << line
          mutex.synchronize { combined_lines << line }
        end
      end

      stderr_thread = Thread.new do
        while (line = stderr.gets)
          stderr_lines << line
          mutex.synchronize { combined_lines << line }
        end
      end

      if wait_thr.join(timeout)
        finished_in_time = true
        exit_status = wait_thr.value.exitstatus
        stdout_thread.join
        stderr_thread.join
      else
        Process.kill("KILL", wait_thr.pid)
        wait_thr.join
        mutex.synchronize do
          stdout_thread.kill
          stderr_thread.kill
        end
      end
    end

    {
      stdout_lines: stdout_lines,
      stderr_lines: stderr_lines,
      combined_lines: combined_lines,
      exit_status: exit_status,
      finished_in_time: finished_in_time
    }
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

  # The sequence of ulimit commands to set the resource usage limits.
  def ulimits
    ""
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
        result = f.read
        # On Windows, we convert newline characters in the file into
        # the Unix-style newline characters used in our test cases.
        if windows?
          result = result.gsub("\r\n", "\n")
        end
        return result
      end
    rescue StandardError
      warn("failed to read '#{filename}'")
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
    File.write(fname, text)
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
  def warning?(expected_message = nil)
    if expected_message.nil?
      @stdout.include?("Warning:")
    else
      @cleaned_stdout =~ Regexp.new("Warning: .*#{Regexp.escape(expected_message)}")
    end
  end

  # true if the FORM job put preprocessor errors.
  def preprocess_error?(expected_message = nil)
    if expected_message.nil?
      @stdout =~ /(^|\R)\S+ Line \d+ ==>/
    else
      @cleaned_stdout =~ Regexp.new("(^|\\R)\\S+ Line \\d+ ==> .*#{Regexp.escape(expected_message)}")
    end
  end

  # true if the FORM job put compile-time errors.
  def compile_error?(expected_message = nil)
    if expected_message.nil?
      @stdout =~ /(^|\R)\S+ Line \d+ -->/
    else
      @cleaned_stdout =~ Regexp.new("(^|\\R)\\S+ Line \\d+ --> .*#{Regexp.escape(expected_message)}")
    end
  end

  # true if the FORM job put run-time errors.
  # NOTE: indeed this implementation detects abnormal terminations
  # via "Terminate()", which also happens for preprocessor/compiler errors.
  def runtime_error?(expected_message = nil)
    if serial?
      result = @stdout =~ /Program terminating at \S+ Line \d+ -->/
    elsif threaded?
      result = @stdout =~ /Program terminating in thread \d+ at \S+ Line \d+ -->/
    elsif mpi?
      result = @stdout =~ /Program terminating in process \d+ at \S+ Line \d+ -->/
    end
    if expected_message.nil?
      result
    else
      # NOTE: it just tests if the output contains the expected message,
      # which is probably put before "Terminate()" is called.
      result && @cleaned_stdout =~ Regexp.new(Regexp.escape(expected_message))
    end
  end

  # true if the FORM job completed without any warnings/errors and
  # the exit code was 0.
  def succeeded?
    if finished? && !warning? && !preprocess_error? && !compile_error? && !runtime_error? && return_value == 0
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
    Regexp.new("^#{san_str}$")
  end
end

# Information of a test case.
class TestInfo
  def initialize
    @classname = nil
    @where = nil    # where the test is defined
    @foldname = nil # fold name of the test
    @enabled = nil  # enabled or not
    @sources = []   # FORM sources
    @time_dilation = nil

    @status = nil   # status
    @times = nil    # elapsed time (array)
  end

  attr_accessor :classname, :where, :foldname, :enabled, :sources, :time_dilation,
                :status, :times

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
      info = @classes_info["Test_#{c}"]
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
    outname = "#{File.basename(filename, '.frm')}.rb"
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
        ulimits = nil
        time_dilation = nil

        infile.each_line do |line|
          line.chop!
          lineno += 1
          if level == 0
            case line
            when /^\*..#\[\s*([^:]*)/
              # fold open: start a class
              fold = $1.strip
              if fold.empty?
                fatal("empty fold", inname, lineno)
              end
              classname = canonical_name(fold)
              info = TestInfo.new
              @classes.push(classname)
              @classes_info["Test_#{classname}"] = info
              info.classname = classname
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
              ulimits = nil
              time_dilation = nil
              if skipping
                line = ""
              else
                line = "class Test_#{classname} < Test::Unit::TestCase; include FormTest"
              end
            when /^\*..#\]/
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
                requires = requires.map { |s| "(#{s})" }.join(" && ")
                line += "def requires; #{requires} end; "
                line += "def requires_str; %(#{requires}) end; "
              end
              if !pendings.nil?
                pendings = pendings.map { |s| "(#{s})" }.join(" || ")
                line += "def pendings; #{pendings} end; "
                line += "def pendings_str; %(#{pendings}) end; "
              end
              if !prepares.nil?
                prepares = prepares.join("; ")
                line += "def prepare; #{prepares} end; "
              end
              if !ulimits.nil?
                ulimits.map! { |s| "ulimit #{s}; " }
                ulimits = ulimits.join
                line += "def ulimits; %(#{ulimits}) end; "
              end
              if !time_dilation.nil?
                line += "def timeout; super() * #{time_dilation} end;"
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

              block += "#{line}\n"
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
          elsif heredoc.nil? && line =~ /^\s*#\s*ulimit\s+(.*)/
            # #ulimit <limits>
            # Example: #ulimit -v 4_000_000
            line = ""
            if ulimits.nil?
              ulimits = []
            end
            ulimits << $1.gsub(/(?<=\d)_(?=\d)/, "") # remove decimal marks (underscores)
          elsif heredoc.nil? && line =~ /^\s*#\s*time_dilation\s+(.*)/
            # #time_dilation <dilation>
            line = ""
            if !time_dilation.nil?
              fatal("attempted to set time_dilation twice", inname, lineno)
            end
            time_dilation = $1.to_f
            if time_dilation <= 0
              fatal("invalid time_dilation", inname, lineno)
            end
            info.time_dilation = time_dilation
          elsif heredoc.nil? && line =~ /^\*\s*#\s*(require|prepare|pend_if|ulimit|time_dilation)\s+(.*)/
            # *#<special instruction>, commented out in the FORM way
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
              block += "#{line}\n"
              blockno += 1
              line = nil
            end
          end
          if !line.nil?
            outfile.write("#{line}\n")
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
        s = @name_patterns[i].to_s.gsub("*", ".*").tr("?", ".")
        s = "^#{s}$"
        @name_patterns[i] = Regexp.new(s)
      end
    end
    @exclude_patterns.length.times do |i|
      if !@exclude_patterns[i].is_a?(Regexp)
        s = @exclude_patterns[i].to_s.gsub("*", ".*").tr("?", ".")
        s = "^#{s}$"
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

    # check --exclude NAME
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
      s = "#{prefix}_#{i}"
    end
    s
  end

  # Delete a test.
  def delete(classname)
    @classes.delete(classname)
    @classes_info.delete("Test_#{classname}")
    # It seems difficult to delete a class.
    # Instead, remove the test method.
    klass = Object.const_get(:"Test_#{classname}")
    klass.send(:remove_method, :"test_#{classname}")
  end
end

# FORM configuration.
class FormConfig
  def initialize(form, mpirun, mpirun_opts, valgrind, valgrind_opts, wordsize, ncpu, timeout, retries, stat, full, verbose, show_newlines)
    @form     = form
    @mpirun   = mpirun
    @mpirun_opts = mpirun_opts
    @valgrind = valgrind
    @valgrind_opts = valgrind_opts
    @ncpu     = ncpu
    @timeout  = timeout
    @retries  = retries
    @stat     = stat
    @full     = full
    @verbose  = verbose
    @show_newlines = show_newlines

    @form_bin      = nil
    @mpirun_bin    = nil
    @valgrind_bin  = nil
    @valgrind_supp = nil

    @head        = nil
    @is_serial   = nil
    @is_threaded = nil
    @is_mpi      = nil
    @wordsize    = wordsize
    @form_cmd    = nil
  end

  attr_reader :form, :mpirun, :mpirun_opts, :valgrind, :valgrind_opts, :ncpu, :timeout, :retries, :stat, :full, :verbose, :show_newlines,
              :form_bin, :mpirun_bin, :valgrind_bin, :valgrind_supp, :head, :wordsize, :form_cmd

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
    if File.executable?(bin)
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
      File.write(frmname, <<-TEST_FRM)
        #-
        Off finalstats;
        .end
      TEST_FRM

      @head = ""
      out, _status = Open3.capture2e("#{@form_bin} #{frmname}")
      out.split("\n").each do |output_line|
        if output_line =~ /FORM/
          @head = output_line
          break
        end
      end

      case @head
      when /^FORM/
        @is_serial   = true
        @is_threaded = false
        @is_mpi      = false
      when /^TFORM/
        @is_serial   = false
        @is_threaded = true
        @is_mpi      = false
      when /^ParFORM/
        @is_serial   = false
        @is_threaded = false
        @is_mpi      = true
      else
        system("#{form_bin} #{frmname}")
        fatal("failed to get the version of '#{@form}'")
      end
      # Check the wordsize.
      # Method 1: from the output header.
      # Method 2: 2^64 = 0 (mod 2^64) => 64-bit machine => sizeof(WORD) = 4, etc.
      wordsize1 = nil
      wordsize2 = nil
      if @head =~ /FORM[^(]*\([^)]*\)\s*(\d+)-bits/
        wordsize1 = $1.to_i / 16
      end
      frmname2 = File.join(tmpdir, "ws.frm")
      File.write(frmname2, <<-TEST_FRM)
        #do w={2,4,8}
          #message wordtest,`w',{2^(`w'*16-1)},{2^(`w'*16)}
        #enddo
        .end
      TEST_FRM
      out, _err, status = Open3.capture3("#{@form_bin} #{frmname2}")
      if status.success?
        out.split("\n").each do |output_line|
          if output_line =~ /~~~wordtest,(\d+),(-?\d+),(-?\d+)/
            w = $1.to_i
            x = $2.to_i
            y = $3.to_i
            if x != 0 && y == 0
              wordsize2 = w
              break
            end
          end
        end
      end
      if !@wordsize.nil?
        if !wordsize1.nil? && @wordsize != wordsize1
          warn("--wordsize=#{@wordsize} specified but the header of '#{@form}' indicates the wordsize = #{wordsize1}")
        end
        if !wordsize2.nil? && @wordsize != wordsize2
          warn("--wordsize=#{@wordsize} specified but the preprocessor calculator of '#{@form}' determined wordsize = #{wordsize2}")
        end
      end
      if @wordsize.nil?
        if !wordsize1.nil? && !wordsize2.nil? && wordsize1 != wordsize2
          warn("the header of '#{@form}' indicates the wordsize = #{wordsize1} but the preprocessor calculator determined wordsize = #{wordsize2}")
        elsif !wordsize1.nil?
          @wordsize = wordsize1
        else
          @wordsize = wordsize2
        end
      end
      if @wordsize.nil?
        warn("failed to get the wordsize of '#{@form}'")
        warn("assuming wordsize = 4")
        @wordsize = 4
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
        if !@mpirun_opts.nil?
          cmdlist << @mpirun_opts
        end
      end
      if !@valgrind_bin.nil?
        cmdlist << @valgrind_bin
        cmdlist << "--leak-check=full"
        if !@valgrind_supp.nil?
          cmdlist << "--suppressions=#{@valgrind_supp}"
        end
        if !@valgrind_opts.nil?
          cmdlist << @valgrind_opts
        end
      end
      cmdlist << @form_bin
      if @is_threaded
        cmdlist << "-w#{@ncpu}"
      end
      @form_cmd = cmdlist.join(" ")
      # Check the output header.
      out, err, status = Open3.capture3("#{@form_cmd} #{frmname}")
      if status.success?
        form_version_line = out.split("\n").first
        if form_version_line.nil?
          warn("failed to get the actual version of FORM")
        else
          @head = form_version_line
        end
      else
        fatal("failed to execute '#{@form_cmd}'")
      end
      if !@valgrind.nil?
        # Include Valgrind version information.
        valgrind_version_line = err.split("\n").select { |line| line.include?("Valgrind") }.first
        if valgrind_version_line.nil?
          warn("failed to get the version of Valgrind")
        else
          @head += "\n#{valgrind_version_line}"
        end
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

  "#{newpath}:#{oldpath}"
end

# Parse `TEST=...`.
def parse_def(pat)
  if pat =~ /^TEST=(.*)/
    return $1
  end

  nil
end

# Parse GROUPID/GROUPCOUNT
def parse_group(group)
  if group =~ %r{^(\d+)/(\d+)$}
    group_id = $1.to_i
    group_count = $2.to_i
    if group_count <= 0
      fatal("group count must be positive: '#{group}'")
    end
    if group_id <= 0 || group_id > group_count
      fatal("group id out of range: '#{group}'")
    end
    return group_id, group_count
  end

  fatal("unrecognized group specification: '#{group}'")
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
  opts.mpirun_opts = nil
  opts.ncpu = 4
  opts.timeout = nil
  opts.retries = 1
  opts.stat = false
  opts.full = false
  opts.enable_valgrind = false
  opts.valgrind = "valgrind"
  opts.valgrind_opts = nil
  opts.wordsize = nil
  opts.dir = nil
  opts.name_patterns = []
  opts.exclude_patterns = []
  opts.group_id = nil
  opts.group_count = nil
  opts.files = []
  opts.verbose = false
  opts.show_newlines = false

  parser = OptionParser.new
  parser.banner = "Usage: #{File.basename($0)} [options] [--] [binname] [files|tests..]"
  parser.on("-h", "--help",
            "Show this help and exit")            { puts(parser); exit }
  parser.on("-l", "--list",
            "List all tests and exit")            { opts.list = true }
  parser.on("--path PATH",
            "Use PATH for executables")           { |path| opts.path = add_path(opts.path, path) }
  parser.on("--form BIN",
            "Use BIN as FORM executable")         { |bin| opts.form = bin }
  parser.on("--mpirun BIN",
            "Use BIN as mpirun executable")       { |bin| opts.mpirun = bin }
  parser.on("--mpirun-opts OPTS",
            "Pass command line options OPTS to mpirun") { |s| opts.mpirun_opts = s }
  parser.on("-w", "--ncpu N",
            "Use N CPUs")                         { |n| opts.ncpu = n.to_i }
  parser.on("-t", "--timeout N",
            "Timeout N in seconds")               { |n| opts.timeout = n.to_i }
  parser.on("-r", "--retries N",
            "Retry up to N times when timeout")   { |n| opts.retries = n.to_i }
  parser.on("-s", "--stat",
            "Print detailed statistics")          { opts.stat = true }
  parser.on("-f", "--full",
            "Full test, ignoring pending")        { opts.full = true }
  parser.on("--enable-valgrind",
            "Enable Valgrind")                    { opts.enable_valgrind = true }
  parser.on("--valgrind BIN",
            "Use BIN as Valgrind executable")     { |bin| opts.enable_valgrind = true; opts.valgrind = bin }
  parser.on("--valgrind-opts OPTS",
            "Pass command line options OPTS to valgrind") { |s| opts.valgrind_opts = s }
  parser.on("--wordsize N",
            "Set the word size")                  { |n| opts.wordsize = n.to_i }
  parser.on("-C", "--directory DIR",
            "Directory for test cases")           { |dir| opts.dir = search_dir(dir, opts) }
  parser.on("-n", "--name NAME",
            "Run tests matching NAME")            { |pat| opts.name_patterns << pat }
  parser.on("-x", "--exclude NAME",
            "Do not run tests matching NAME")     { |pat| opts.exclude_patterns << pat }
  parser.on("-g", "--group GROUPID/GROUPCOUNT",
            "Split tests and run only one group") { |group| opts.group_id, opts.group_count = parse_group(group) }
  parser.on("-v", "--verbose",
            "Enable verbose output")              { opts.verbose = true }
  parser.on("--show-newlines",
            "Show newline characters")            { opts.show_newlines = true }
  parser.on("-D TEST=NAME",
            "Alternative way to run tests NAME")  { |pat| opts.name_patterns << parse_def(pat) }
  begin
    parser.parse!(ARGV)
  rescue OptionParser::ParseError => e
    $stderr.puts(e.backtrace.first + ": #{e.message} (#{e.class})")
    e.backtrace[1..-1].each { |m| $stderr.puts("\tfrom #{m}") }
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

  # Split tests into groups and run only one group.
  if opts.group_id
    infos = FormTest.tests.classes_info_list
    total = infos.length
    divided = total / opts.group_count
    reminder = total - divided * opts.group_count

    test_nos = []
    n = 0

    (1..opts.group_count).each do |i|
      (1..divided).each do
        if i == opts.group_id
          test_nos.push(n)
        end
        n += 1
      end
      if i <= reminder
        if i == opts.group_id
          test_nos.push(n)
        end
        n += 1
      end
    end

    infos = FormTest.tests.classes_info_list
    infos.each_with_index do |info, i|
      if !test_nos.include?(i)
        FormTest.tests.delete(info.classname)
      end
    end
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
    ENV["PATH"] = "#{opts.path}#{File::PATH_SEPARATOR}#{ENV.fetch('PATH', '')}"
  end

  # Set FORMPATH
  ENV["FORMPATH"] = File.expand_path(opts.dir.nil? ? TESTDIR : opts.dir) +
                    (ENV["FORMPATH"].nil? ? "" : ":#{ENV['FORMPATH']}")

  # Default mpirun_opts.
  if opts.mpirun_opts.nil?
    opts.mpirun_opts = DEFAULT_MPIRUN_OPTS
  end

  # Default valgrind_opts.
  if opts.valgrind_opts.nil?
    opts.valgrind_opts = DEFAULT_VALGRIND_OPTS
  end

  # Default timeout.
  if opts.timeout.nil?
    opts.timeout = DEFAULT_TIMEOUT
    # Running Valgrind can be really slow.
    if opts.enable_valgrind
      opts.timeout *= VALGRIND_TIME_DILATION
    end
  end

  # Initialize the FORM configuration.
  FormTest.cfg = FormConfig.new(opts.form,
                                opts.mpirun,
                                opts.mpirun_opts,
                                opts.enable_valgrind ? opts.valgrind : nil,
                                opts.valgrind_opts,
                                opts.wordsize,
                                opts.ncpu,
                                [opts.timeout, 1].max,
                                [opts.retries, 1].max,
                                opts.stat,
                                opts.full,
                                opts.verbose,
                                opts.show_newlines)
  FormTest.cfg.check
  puts("Check #{FormTest.cfg.form_bin}")
  puts(FormTest.cfg.head)
end

def need_output_detailed_statistics?
  # Check if already done.
  $output_detailed_statistics_done ||= false
  return false if $output_detailed_statistics_done

  # Check if --stat enabled.
  return false if FormTest.cfg.nil? || !FormTest.cfg.stat

  # Check if enabled test cases exist.
  return false if FormTest.tests.classes_info_list.empty?

  true
end

def output_detailed_statistics(output = nil)
  return if !need_output_detailed_statistics?

  $output_detailed_statistics_done = true

  # method to output
  output ||= method(:puts)

  # Print detailed statistics.

  term_width = IO.console_size[1]

  infos = FormTest.tests.classes_info_list
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

  infos.each do |info|
    (0..info.sources.length - 1).each do |i|
      t = 0
      if !info.times.nil? && i < info.times.length
        t = info.times[i]
      end
      timeout = FormTest.cfg.timeout
      if !info.time_dilation.nil?
        timeout *= info.time_dilation
      end
      if i == 0
        s = format("%s %s  %s %s%s",
                   lpad(info.foldname, max_foldname_width),
                   lpad("(#{info.where})", max_where_width),
                   lpad(info.status.nil? ? "UNKNOWN" : info.status, status_width),
                   bar_str(t, timeout, bar_width),
                   format_time(t, timeout))
      else
        s = format("%s %s  %s %s%s",
                   lpad("", max_foldname_width),
                   lpad("", max_where_width),
                   lpad("", status_width),
                   bar_str(t, timeout, bar_width),
                   format_time(t, timeout))
      end
      if !info.status.nil? && info.status == "FAILED"
        begin
          output.call(s, color("failure"))
        rescue StandardError
          output.call(s)
        end
      else
        output.call(s)
      end
    end
  end

  output.call("Default timeout: #{FormTest.cfg.timeout}s")

  true
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
  t %= 3600
  m = Integer(t / 60)
  t %= 60
  s = Integer(t)
  t %= 1
  ms = Integer(t * 1000)
  format("%s%02d:%02d:%02d.%03d", overflow ? ">" : " ", h, m, s, ms)
end

if $0 == __FILE__
  main
end
