cl = sbcl
clflags = --noinform
lisp = $(cl) $(clflags)

src = *.asd src/*.lisp
dir = $(shell pwd)
bin = antimer
system = antimer

PREFIX=/usr
bindir=$(PREFIX)/bin
bindest=$(bindir)/$(bin)

all: $(bin)

$(bin): $(src)
	$(lisp) --eval "(asdf:load-system :$(system))" \
		--eval "(asdf:compile-system :$(system) :force t)" \
	        --eval "(setf uiop:*image-entry-point* #'antimer.cli:main)" \
		--eval "(uiop:dump-image \"$(dir)/$(bin)\" :executable t)"

clean:
	rm $(bin)

install: all
	@echo "Installing to $(bindest)"
	mkdir -p $(bindir)
	install -m 755 $(bin) $(bindest)
	chmod 755 $(bindest)

uninstall:
	@echo "Uninstalling from $(bindest)"
	rm -f $(bindest)
