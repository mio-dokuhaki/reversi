FILE = atcoder
make:
	rlwrap sbcl --control-stack-size 1024 --dynamic-space-size 1024 --userinit ~/.sbclrc --load $(FILE).lisp --quit

clisp:
	clisp $(FILE).lisp

# remove fasl files
clean:
	rm -rf *.fasl

# Makefile to setup a Common Lisp project with cl-project

# Default author name
AUTHOR = DefaultAuthor

# Default directory name
DIR = DefaultProject

.PHONY: setup-project
setup-project:
	rlwrap sbcl --non-interactive --eval '(ql:quickload "cl-project")' \
		--eval '(cl-project:make-project #P"$(DIR)/" :author "$(AUTHOR)")' \
		--eval '(quit)'

silicon:
	silicon $(FILE).lisp -o $(FILE).png -f 'SauceCodePro Nerd Font Mono; 游教科書体'

compile:
	ros build $(FILE).lisp

run:
	ros run $(FILE).lisp
