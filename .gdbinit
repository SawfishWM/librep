# $Id$

# prints $$ to standard output
define v
set print_val(cmd_stdout_file(), $), stream_putc(cmd_stdout_file(), '\n')
end

