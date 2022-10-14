LISP ?= sbcl

build:
	$(LISP) --load doors-main.asd \
		--eval '(ql:quickload :doors-main)' \
		--eval '(asdf:make :doors-main)' \
		--eval '(quit)'

clean:
	@rm doors

all: build
