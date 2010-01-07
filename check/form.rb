# this file is to be included (required) in the actual tests

require 'open3'     # for popen3() to run FORM
require 'rbconfig'  # for Config::CONFIG to determine OS
require 'test/unit' # for the unit test classes
if RUBY_VERSION =~ /1\.8\..+/ # only for version 1.8.x
	require 'test/unit/ui/console/testrunner'
end

# names of the FORM executables
FORM = "form"
TFORM = "tform"

# maximal running time in seconds of FORM jobs before they get terminated
TIMEOUT = 10

# names of temporary files
TMPFILES_BASENAME = "form_rb_exam"
TESTPRG_NAME = TMPFILES_BASENAME+".frm"
LOGFILE_NAME = TMPFILES_BASENAME+".log"
STRACETMP_NAME = TMPFILES_BASENAME+"_strace"

def cleanup_pidfiles
	Dir.glob(STRACETMP_NAME+".*") { |fn| File.delete(fn) }
end
def cleanup_tempfiles
	File.delete(TESTPRG_NAME) if File.exist?(TESTPRG_NAME)
	File.delete(LOGFILE_NAME) if File.exist?(LOGFILE_NAME)
	cleanup_pidfiles
end

# determine OS we are running on
osname = Config::CONFIG["target_os"]
if osname =~ /linux/
	LINUX = true
elsif osname =~ /mswin32/
	WINDOWS = true
end

class FormTest < Test::Unit::TestCase
	def initialize(test_method_name)
		super(test_method_name)
		@input_file = ""
		@extra_parameter = ""
		@interactive = false
		cleanup_pidfiles
	end

	# Functions to specify test-run input and environment
	def input(str)
		File.open(TESTPRG_NAME, "w") { |f| f.write(str) }
		@input_file = TESTPRG_NAME
	end
	def input_file(str)
		@input_file = str
	end
	def extra_parameter(str)
		@extra_parameter = str
	end
	def interactive(true_or_false)
		@interactive = true_or_false
	end

	# Automatically called at end of each test run
	def teardown
		remove_files
	end
	def remove_files
		cleanup_tempfiles if passed?
	end

	# To run a (T)FORM job
	def execute(executable, timeout=TIMEOUT)
		cmdline = "strace -ff -o "+STRACETMP_NAME+" "+executable+" "+@extra_parameter+" "+@input_file
		@stdout = []; @stderr = []
		begin
			runner = Thread.current
			killer = Thread.new(timeout) { |timeout| sleep timeout; runner.raise }
			@inputstream, @outputstream, @errorstream = Open3.popen3(cmdline)
			out = Thread.new { while (( line = @outputstream.gets )) do @stdout << line end }
			err = Thread.new { while (( line = @errorstream.gets )) do @stderr << line end }
			out.join
			err.join
			killer.kill
		rescue
			@unfinished = true
		else
			@unfinished = false
		ensure
			@inputstream.close
			@outputstream.close
			@errorstream.close
			@stdout = @stdout.join
			@stderr = @stderr.join
			tmpfiles = Dir.glob(STRACETMP_NAME+".*")
			if tmpfiles.size == 0 then raise "Error: could not run executable #{executable}!"
			elsif tmpfiles.size > 1 then puts "Warning: Cannot terminate or evaluate FORM job!"
			else pid = File.extname(tmpfiles[0])[1..-1] end
			if @unfinished
				puts "Timeout: Terminating FORM job!"
				system("kill -SIGKILL "+pid)
			end
		end
		evaluate pid
	end

	# Prepares data for test-result functions
	def evaluate(pid)
		File.open(STRACETMP_NAME+"."+pid, "r") { |f| @strace_out = f.readlines.join }
	end

	# Test-result functions 
	def return_value
		@strace_out =~ /exit_group\((\d+)\)/
		$1.to_i
	end
	def result(exprname)
		@stdout =~ /.+\n(\s+#{exprname}\s*=.+;\n)/m
		return $1
	end

	# Test-result functions to be used in assertions
	def crash
		@strace_out =~ /Segmentation fault/
	end
	def no_crash; !crash; end
	def warning
		@stdout =~ /--> Warning:/
	end
	def no_warning; !warning; end
	def compile_error
		@stdout =~ /#{TESTPRG_NAME} Line \d+ -->/
	end
	def no_compile_error; compile_error; end
	def runtime_error
		@stdout =~ /Program terminating at #{TESTPRG_NAME} Line \d+ -->/
	end
	def no_runtime_error; !runtime_error; end
	def error
		compile_error || runtime_error || @stderr != ""
	end
	def no_error; !error; end
	def has_not_finished
		@unfinished
	end
	def has_finished
		!@unfinished
	end
	def problem
		has_not_finished || warning || error || crash
	end
	def no_problem; !problem; end

	# Utility functions for pattern matching
	def ExactPattern(str)
		san_str = Regexp.quote(str)
		Regexp.new(san_str)
	end
	def Pattern(str)
		san_str = Regexp.quote(str)
		san_str = san_str.gsub(/(\\n|^)(\\ |\\t)+/, '\1\s+')
		Regexp.new(san_str, Regexp::MULTILINE)
	end
end

if __FILE__ == $0
	if ARGV == []
		if ENV['srcdir']
			path = ENV['srcdir'] + "/**/*.rb"
		else
			path = "**/*.rb"
		end
		files = Dir.glob(path)
	else
		files = ARGV
	end
	files.each do |iname|
		if iname != $0
			require iname
		end
	end
	if RUBY_VERSION =~ /1\.8\..+/
		class DerivedTests
			def self.suite
				suite = Test::Unit::TestSuite.new("(T)FORM tests")
				Class.constants.each do |c|
					cl = Class.class_eval(c)
					if cl.class == Class && cl.ancestors[1] == FormTest
						suite << cl.suite
					end
				end
				return suite
			end
		end
		Test::Unit::UI::Console::TestRunner.run(DerivedTests)
	end
end
