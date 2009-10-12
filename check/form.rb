# this file is to be included (required) in the actual tests

require 'open3'     # for popen3() to run FORM
require 'rbconfig'  # for Config::CONFIG to determine OS
require 'test/unit' # for the unit test classes

# names of the FORM executables
FORM = "form"
TFORM = "tform"

# names of temporary files
TMPFILES_BASENAME = "form_rb_exam"
TESTPRG_NAME = TMPFILES_BASENAME+".frm"
LOGFILE_NAME = TMPFILES_BASENAME+".log"
STRACETMP_NAME = TMPFILES_BASENAME+"_strace.tmp"

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
		if passed?
			File.delete(STRACETMP_NAME) if File.exist?(STRACETMP_NAME)
			File.delete(TESTPRG_NAME) if File.exist?(TESTPRG_NAME)
			File.delete(LOGFILE_NAME) if File.exist?(LOGFILE_NAME)
		end
	end

	# To run a (T)FORM job
	def execute(executable)
		cmdline = "strace -o "+STRACETMP_NAME+" "+executable+" "+@extra_parameter+" "+@input_file
		@inputstream, @outputstream, @errorstream = Open3.popen3(cmdline)
		@stdout = @outputstream.readlines.join
		@stderr = @errorstream.readlines.join
		if !@interactive
			@inputstream.close
			@outputstream.close
			@errorstream.close
			evaluate
		end
	end

	# Prepares data for test-result functions
	def evaluate
		File.open(STRACETMP_NAME, "r") { |f| @strace_out = f.readlines.join }
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
	def problem
		warning || error || crash
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
	Dir.glob(ENV['srcdir']+"/**/*.rb") do |iname|
		if iname != $0
			require iname
		end
	end
end
