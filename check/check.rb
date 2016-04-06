#! /bin/sh
exec ruby -S -x "$0" "$@"
#! ruby

# The default prefix for the root temporary directory.
TMPDIR_PREFIX = "form_check_"

# The default maximal running time in seconds of FORM jobs before they get
# terminated.
TIMEOUT = 10

# The default directory for searching test cases.
TESTDIR = File.dirname(__FILE__)

# Check the Ruby version.
if RUBY_VERSION < "1.8.0"
  warn("ruby 1.8 required for the test suite")
  exit
end

# Register the cleanup function before loading test/unit.
at_exit { cleanup }

require "open3"
require "ostruct"
require "optparse"
require "set"
require "tmpdir"

# We use test/unit, which is now not in the standard library.
begin
  require "test/unit"
rescue LoadError
  def cleanup
  end
  warn("test/unit required for the test suite")
  exit
end

# The root temporary directory.
$tmpdir = nil

# Show an error message and exit.
def fatal(message, file=nil, lineno=nil)
  if file != nil && lineno != nil
    STDERR.puts("#{file}:#{lineno}: error: #{message}")
  elsif file != nil
    STDERR.puts("#{file}: error: #{message}")
  else
    STDERR.puts("error: #{message}")
  end
  exit(1)
end

# Show a warning message.
def warn(message, file=nil, lineno=nil)
  if file != nil && lineno != nil
    STDERR.puts("#{file}:#{lineno}: warning: #{message}")
  elsif file != nil
    STDERR.puts("#{file}: warning: #{message}")
  else
    STDERR.puts("warning: #{message}")
  end
end

# Clean up the temporary directory specified by the global variable $tmpdir.
def cleanup
  if $tmpdir != nil
    rm($tmpdir)
    50.times do  # up to 5 seconds
      if !FileTest.directory?($tmpdir) then
        break
      end
      sleep(0.1)
      rm($tmpdir)
    end
    if FileTest.directory?($tmpdir) then
      warn("failed to delete the temporary directory '#{$tmpdir}'")
    end
  end
end

# Create a temporary directory and return the name.
def mktmpdir(prefix)
  1000.times do |i|
    dir = prefix + (Process.pid.to_s + i.to_s + Time.now.to_s).hash.abs.to_s
    begin
      if Dir.mkdir(dir, 0700) == 0
        return dir
      end
    rescue
    end
  end
  fatal("failed to make a temporary directory starting with '#{prefix}'")
end

# Delete the given file/directory.
def rm(file)
  if FileTest.directory?(file) then
    Dir.foreach(file) do |f|
      next if /^\.+$/ =~ f
      rm(file.sub(/\/+$/,"") + "/" + f)
    end
    begin
      Dir.rmdir(file)
    rescue
    end
  elsif FileTest.exists?(file)
    begin
      File.delete(file)
    rescue
    end
  end
end

# Here create the root temporary directory.
$tmpdir = mktmpdir(File.join(Dir.tmpdir, TMPDIR_PREFIX))

# To be mixed-in all FORM tests.
module FormTest
  # The interplay with globals (static members).

  def self.cfg=(val)
    @@cfg = val
  end

  def self.cfg
    @@cfg
  end

  def self.tests=(val)
    @@tests = val
  end

  def self.tests
    @@tests
  end

  def self.tmproot
    $tmpdir
  end

  # Accessor to the configuration.

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

  def wordsize
    FormTest.cfg.wordsize
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
    @tmpdir = mktmpdir(File.join(FormTest.tmproot, "test"))
    nfiles.times do |n0|
      n = n0 + 1
      open(File.join(@tmpdir, "#{n}.frm"), "w") do |file|
        file.write(FormTest.tests.source(self.class.name, n))
      end
    end
  end

  # Delete the working directory.
  def cleanup_files
    if @tmpdir != nil
      rm(@tmpdir)
    end
    @tmpdir = nil
  end

  # Called from derived classes' test_* methods
  def do_test
    if requires
      setup_files
      @stdout = ""
      @stderr = ""
      begin
        nfiles.times do |n0|
          n = n0 + 1
          @filename = "#{n}.frm"
          execute("#{FormTest.cfg.form_cmd} #{@filename}")
          if not finished?
            assert(false, "timeout (= #{timeout} sec) in #{@filename} of #{self.name_of_test}")
          end
          if return_value != 0
            break
          end
        end
        yield
      rescue Exception => e
        STDERR.puts()
        STDERR.puts("=" * 79)
        if nfiles <= 1
          STDERR.puts("#{self.name_of_test} FAILED")
        else
          STDERR.puts("#{self.name_of_test} (#{n}/${nfiles}) FAILED")
        end
        STDERR.puts("=" * 79)
        STDERR.puts(@stdout)
        STDERR.puts("=" * 79)
        STDERR.puts()
        raise e
      end
    else
      if defined? omit
        omit(requires_str) do
          yield
        end
      elsif defined? skip
        skip(requires_str)
      end
    end
  end

  # Execute a FORM job.
  def execute(cmdline)
    @finished = false
    @exit_status = nil
    execute_popen3(cmdline, timeout)
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
        while line = stdoutstream.gets do
          stdout << line
        end
      end
      err = Thread.new do
        while line = stderrstream.gets do
          stderr << line
        end
      end
      begin
        runner = Thread.current
        killer = Thread.new(timeout) do |timeout|
          sleep(timeout)
          runner.raise
        end
        out.join
        err.join
        killer.kill
      rescue
        while out.alive? && stdout.empty? do
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
        out.kill  # avoid SEGFAULT at IO#close in some old versions
        err.kill
      end
    end

    if !stdout.empty? && stdout[0] =~ /pid=([0-9]+)/
      stdout.shift
    end

    if @finished && !stdout.empty? && stdout[-1] =~ /exit_status=([0-9]+)/
      @exit_status = $1.to_i
      stdout.pop
    end

    @stdout += stdout.join
    @stderr += stderr.join
  end

  # Default assertions.
  def default_check
    if return_value != 0
      assert(false, "nonzero return value (= #{return_value}) from #{@filename} of #{self.name_of_test}")
    elsif warning?
      assert(false, "warning in #{@filename} of #{self.name_of_test}")
    else
      assert(true)
    end
  end

  # The name of the test.
  def name_of_test
    classname = self.class.name[5..-1]
    "#{FormTest.tests.classes_testname[classname]} (#{FormTest.tests.classes_at[classname]})"
  end

  # Methods to be overridden in derived classes.

  # The number of FORM files attached to the test.
  def nfiles
    1
  end

  # The required condition.
  def requires
    true
  end

  # The string representation for the required condition.
  def requires_str
    "true"
  end

  # Test-result functions.

  # The exit status as a number
  def return_value
    @exit_status
  end

  # The verbatim result keeping line breaks and whitespaces.
  # Must be in the default output format.
  def exact_result(exprname, index=-1)
    matches = @stdout.scan(/^[ \t]+#{exprname}\s*=(.+?);/m)
    return matches[index].first if not matches.empty? and not matches[index].nil?
    return ""
  end

  # The result on one line with multiple whitespaces reduced to one.
  # Must be in the default output format.
  def result(exprname, index=-1)
    r = exact_result(exprname, index)
    return r.gsub(/\s+/, "") if not r.nil?
    return ""
  end

  # The size in byte.
  # Must be in the default statistics format.
  def bytesize(exprname, index=-1)
    matches = @stdout.scan(/^[ \t]+#{exprname}\s*Terms in output\s*=\s*\d+\s*Bytes used\s*=\s*(\d+)/m)
    return matches[index].first.to_i if not matches.empty? and not matches[index].nil?
    return -1
  end

  # The file contents as a string.
  def file(filename)
    begin
      open(File.join(@tmpdir, filename), "r") do |f|
        return f.read
      end
    rescue
      STDERR.puts("warning: file not found '#{filename}'")
      return ""
    end
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
    finished? && !warning? && !compile_error? && !runtime_error? &&
    @stderr.empty? && return_value == 0
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

# List of test cases.
class TestCases
  def initialize
    @files = []             # ruby files
    @classes = []           # test classes (unsorted)
    @classes_set = Set.new  # set of test classes
    @classes_at = {}        # where tests are defined
    @classes_testname = {}  # fold name of test classes
    @sources = {}           # form sources

    @name_patterns = []
    @exclude_patterns = []
  end

  attr_accessor :name_patterns, :exclude_patterns
  attr_reader :classes, :classes_at, :classes_testname

  # Convert a .frm file to a .rb file and load it.
  def make_ruby_file(filename)
    # Check existing files.
    inname = File.basename(filename)
    outname = File.basename(filename, ".frm") + ".rb"
    if @files.index(outname) != nil
      fatal("duplicate output file name", inname)
    end
    @files.push(outname)

    outname = File.join($tmpdir, outname)

    open(filename, "r") do |infile|
      open(outname, "w") do |outfile|
        lineno = 0
        level = 0
        classname = nil
        block = nil
        blockno = 0
        fileno = 0
        skipping = false
        heredoc = nil
        requires = nil

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
              @classes.push(classname)
              @classes_set.add(classname)
              @classes_at[classname] = "#{inname}:#{lineno.to_s}"
              @classes_testname[classname] = fold

              level += 1
              block = ""
              blockno = 0
              fileno = 0
              skipping = !test_enabled?(classname)
              heredoc = nil
              requires = nil
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
          else
            if heredoc == nil && line =~ /^\*..#\]\s*([^:]*)/ && level == 1
              # fold close: end of the class
              fold = $1.strip
              foldname = @classes_testname[classname]
              if !fold.empty? && fold != foldname
                warn("unmatched fold '#{fold}', which should be '#{foldname}'", inname, lineno)
              end

              if skipping
                line = ""
              else
                line = ""
                if fileno == 0
                  # no .end
                  blockno.times do
                    outfile.write("\n")
                  end

                  block += ".end\n"
                  fileno += 1
                  @sources["Test_#{classname}##{fileno.to_s}"] = block

                  line += "def test_#{classname}; do_test { default_check } end; "
                else
                  outfile.write("def test_#{classname}; do_test {\n" + block)
                  line = "} end; "
                end
                line += "def nfiles; #{fileno.to_s} end; " if fileno != 1
                if requires != nil
                  requires = requires.map{|s| "(" + s + ")"}.join(" and ")
                  line += "def requires; #{requires} end; "
                  line += "def requires_str; %&#{requires}& end; "
                end
                line += "end"
              end
              level = 0
            elsif heredoc == nil && line =~ /^\s*\.end/
              # .end
              if skipping
                line = ""
              else
                blockno += 1 if fileno > 0  # previous .end
                blockno.times do
                  outfile.write("\n")
                end

                block += line + "\n"
                fileno += 1
                @sources["Test_#{classname}##{fileno.to_s}"] = block

                block = ""
                blockno = 0

                line = nil  # later
              end
            elsif heredoc == nil && line =~ /^\s*#\s*require\s+(.*)/
              # #require
              line = ""
              if requires == nil
                requires = []
              end
              requires << $1
            elsif heredoc == nil && line =~ /^\*\s*#\s*require\s+(.*)/
              # *#require (commented out in the FORM way)
              line = ""
            else
              if heredoc == nil
                if line =~ /^\*..#\[/
                  # fold open
                  level += 1
                elsif line =~ /^\*..#\]\s*([^:]*)/
                  # fold close
                  level -= 1
                end
                if line =~ /<</
                  if line =~ /<</ && (line =~ /<<-?(\w+)/ ||
                                      line =~ /<<-?"(\w+)"/ ||
                                      line =~ /<<-?'(\w+)'/ ||
                                      line =~ /<<-?`(\w+)`/)
                    # start here document
                    heredoc = Regexp.new($1)
                  end
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
          end
          if line != nil
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
      if !@name_patterns[i].kind_of?(Regexp)
        s = @name_patterns[i].to_s.gsub("\*", ".*").gsub("\?", ".")
        s = "^" + s + "$"
        @name_patterns[i] = Regexp.new(s)
      end
    end
    @exclude_patterns.length.times do |i|
      if !@exclude_patterns[i].kind_of?(Regexp)
        s = @exclude_patterns[i].to_s.gsub("\*", ".*").gsub("\?", ".")
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
    return ok
  end

  # Return a FORM source.
  def source(classname, n)
    @sources["#{classname}##{n}"]
  end

  # Return a class name that is valid and unique.
  def canonical_name(name)
    prefix = name.gsub(/[^a-zA-Z0-9_]/, "_")
    s = prefix
    i = 0
    while true do
      if not @classes.include?(s)
        break
      end
      i += 1
      s = prefix + "_" + i.to_s
    end
    return s
  end
end

# FORM configuration.
class FormConfig
  def initialize(form, mpirun, ncpu, timeout)
    @form    = form
    @mpirun  = mpirun
    @ncpu    = ncpu
    @timeout = timeout

    @bin         = nil
    @head        = nil
    @is_serial   = nil
    @is_threaded = nil
    @is_mpi      = nil
    @wordsize    = nil
    @form_cmd    = nil
  end

  attr_reader :form, :mpirun, :ncpu, :timeout, :bin, :head, :wordsize, :form_cmd

  def serial?
    @is_serial
  end

  def threaded?
    @is_threaded
  end

  def mpi?
    @is_mpi
  end

  def check
    # Find a FORM excutable as the shell does.
    @bin = nil
    if @form != File.basename(@form)
      # The relative path to the absolute path.
      @bin = File.expand_path(@form)
    else
      # Search from $PATH.
      for path in ENV["PATH"].split(":") do
        candidate = File.join(path, @form)
        if File.executable?(candidate)
          @bin = File.expand_path(candidate)
          break
        end
      end
    end
    @bin = @form if @bin.nil?  # fallback, probably won't work
    # Check if FORM is available.
    system("cd #{$tmpdir}; type #{@bin} >/dev/null 2>&1")
    if $? != 0
      if @form == @bin
        fatal("executable '#{@form}' not found")
      else
        fatal("executable '#{@form}' ('#{@bin}') not found")
      end
    end
    # Check the FORM version.
    tmpdir = mktmpdir(File.join($tmpdir, "ver_"))
    begin
      frmname = File.join(tmpdir, "ver.frm")
      open(frmname, "w") do |f|
        f.write(<<-'EOF')
#-
Off finalstats;
.end
  EOF
      end
      @head = `#{@bin} #{frmname} 2>/dev/null`.split("\n").first
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
        fatal("failed to get the version of '#{@form}'")
      end
      if @head =~ /FORM[^(]*\([^)]*\)\s*(\d+)-bits/
        @wordsize = $1.to_i / 16
      else
        fatal("failed to get the wordsize of '#{@form}'")
      end
      # Construct the command.
      case
      when @is_serial
        @form_cmd = @bin
      when @is_threaded
        @form_cmd = "#{@bin} -w#{@ncpu}"
      when @is_mpi
        @form_cmd = "#{@mpirun} -np #{@ncpu} #{@bin}"
      end
      @head = `#{@form_cmd} #{frmname} 2>/dev/null`.split("\n").first
      if $? != 0
        fatal("failed to execute '#{@form_cmd}'")
      end
    ensure
      rm(tmpdir)
    end
  end
end

def main
  # Parse options.

  def add_path(oldpath, newpath)
    newpath = File.expand_path(newpath)
    if oldpath.nil?
      return newpath
    else
      return newpath + ":" + oldpath
    end
  end

  def parse_D(pat)
    if pat =~ /^TEST=(.*)/
      return $1
    end
    return nil
  end

  def search_file(file, opts)
    f = file
    return f if File.exist?(f)
    if not opts.dir.nil?
      f = File.join(opts.dir, file)
      return f if File.exist?(f)
    end
    if not TESTDIR.nil?
      f = File.join(TESTDIR, file)
      return f if File.exist?(f)
    end
    fatal("file '#{file}' not found")
  end

  def search_dir(dir, opts)
    d = dir
    return d if File.directory?(d)
    if not opts.dir.nil?  # may be confusing
      d = File.join(opts.dir, dir)
      return d if File.directory?(d)
    end
    if not TESTDIR.nil?
      d = File.join(TESTDIR, dir)
      return d if File.directory?(d)
    end
    fatal("directory '#{dir}' not found")
  end

  opts = OpenStruct.new
  opts.list = false
  opts.path = nil
  opts.form = "form"
  opts.mpirun = "mpirun"
  opts.ncpu = 4
  opts.timeout = TIMEOUT
  opts.dir = nil
  opts.name_patterns = []
  opts.exclude_patterns = []
  opts.files = []

  parser = OptionParser.new
  parser.banner = "Usage: #{File.basename($0)} [options] [binname] [files|tests..]";
  parser.on("-h", "--help",          "Show this help and exit")           { puts parser; exit }
  parser.on("-l", "--list",          "list all tests and exit")           { opts.list = true }
  parser.on(      "--path PATH",     "Use PATH for executables")          { |path| opts.path = add_path(opt_path, path) }
  parser.on(      "--form BIN",      "Use BIN as FORM executable")        { |bin|  opts.form = bin }
  parser.on(      "--mpirun BIN",    "Use BIN as mpirun executable")      { |bin|  opts.mpirun = bin }
  parser.on("-w", "--ncpu N",        "Use N cpus")                        { |n|    opts.ncpu = n.to_i }
  parser.on("-t", "--timeout N",     "Timeout N in seconds")              { |n|    opts.timeout = n.to_i }
  parser.on("-C", "--directory DIR", "Directory for test cases")          { |dir|  opts.dir = search_dir(dir, opts) }
  parser.on("-n", "--name NAME",     "Run tests matching NAME")           { |pat|  opts.name_patterns << pat }
  parser.on("-x", "--exclude NAME",  "Do not run tests matching NAME")    { |pat|  opts.exclude_patterns << pat }
  parser.on("-D TEST=NAME",          "Alternative way to run tests NAME") { |pat|  opts.name_patterns << parse_D(pat) }
  parser.parse!(ARGV)

  # Parse other arguments.

  while ARGV.length > 0
    case
    when ARGV[0] =~ /\.frm$/
      opts.files << search_file(ARGV[0], opts)
    when ARGV[0] =~ /form/ || ARGV[0] =~ /vorm/ || File.executable?(ARGV[0])
      opts.form = ARGV[0]
    else
      opts.name_patterns << ARGV[0]
    end
    ARGV.shift
  end

  # Make test cases.

  FormTest.tests = TestCases.new
  FormTest.tests.name_patterns = opts.name_patterns
  FormTest.tests.exclude_patterns = opts.exclude_patterns

  if opts.files.length == 0
    Dir.glob(File.join(opts.dir.nil? ? TESTDIR : opts.dir, "*.frm")).sort.each do |file|
      opts.files << search_file(file, opts)
    end
  end

  opts.files.uniq.sort.each do |file|
    FormTest.tests.make_ruby_file(file)
  end

  # --list option.
  if opts.list
    FormTest.tests.classes.each do |key|
      puts("#{key} (#{FormTest.tests.classes_at[key]})")
    end
    exit
  end

  # --path option.
  if not opts.path.nil?
    ENV["PATH"] = opt.path + ":" + ENV["PATH"]
  end

  # Set FORMPATH
  ENV["FORMPATH"] = File.expand_path(opts.dir.nil? ? TESTDIR : opts.dir) +
                    (ENV["FORMPATH"].nil? ? "" : ":" + ENV["FORMPATH"])

  # Initialize the FORM configuration.
  FormTest.cfg = FormConfig.new(opts.form, opts.mpirun, opts.ncpu,
                                opts.timeout > 1 ? opts.timeout : 1)
  FormTest.cfg.check
  puts "Check #{FormTest.cfg.bin}"
# puts "FORMPATH=#{ENV["FORMPATH"]}"
# puts "FORMTMP=#{ENV["FORMTMP"]}"
# puts "FORMSETUP=#{ENV["FORMSETUP"]}"
  puts FormTest.cfg.head
end

if __FILE__ == $0
  main
end
