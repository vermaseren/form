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

def ExactPattern(str)
	san_str = Regexp.quote(str)
	return Regexp.new(san_str)
end

def Pattern(str)
	san_str = Regexp.quote(str)
	san_str = san_str.gsub(/(\\n|^)(\\ )+/, '\1\s+')
	return Regexp.new(san_str)
end

class FormTest < Test::Unit::TestCase
	def initialize(test_method_name)
		super(test_method_name)
		@input = ""
		@input_file = ""
		@extra_parameter = ""
		@needs_input = false
	end
	def setup
		if @input_file == ""
			File.open(TESTPRG_NAME, "w") { |f| f.write(@input) }
			@input_file = TESTPRG_NAME
		end
	end
	def teardown
		if passed?
			File.delete(STRACETMP_NAME) if File.exist?(STRACETMP_NAME)
			File.delete(TESTPRG_NAME) if File.exist?(TESTPRG_NAME)
			File.delete(LOGFILE_NAME) if File.exist?(LOGFILE_NAME)
		end
	end
	def execute(executable)
		cmdline = "strace -o "+STRACETMP_NAME+" "+executable+" "+@extra_parameter+" "+@input_file
		@inputstream, @outputstream, @errorstream = Open3.popen3(cmdline)
		@stdout = @outputstream.readlines.join
		@stderr = @errorstream.readlines.join
		if !@needs_input
			@inputstream.close
			@outputstream.close
			@errorstream.close
			evaluate
		end
	end
	def evaluate
		File.open(STRACETMP_NAME, "r") { |f| @strace_out = f.readlines.join }
		@crash = (@strace_out =~ /Segmentation fault/)
		@strace_out =~ /exit_group\((\d+)\)/
		@return_value = $1.to_i
		@warning = (@stdout =~ /--> Warning:/)
		@compile_error = (@stdout =~ /#{TESTPRG_NAME} Line \d+ -->/)
		@runtime_error = (@stdout =~ /Program terminating at #{TESTPRG_NAME} Line \d+ -->/)
		@error = @compile_error || @runtime_error || @stderr != ""
		@problem = @warning || @error || @crash
		@no_crash = !@crash
		@no_warning = !@warning
		@no_compile_error = !@compile_error
		@no_runtime_error = !@runtime_error
		@no_error = !@error
		@no_problem = !@problem
	end
	def result(exprname)
		@stdout =~ /.+\n(\s+#{exprname}\s*=.+;\n)/m
		return $1
	end
end
