cl = sbcl
clflags = --noinform
lisp = $(cl) $(clflags)

src = *.asd src/*.lisp
bin = antimer
system = antimer

PREFIX=/usr
bindir=$(PREFIX)/bin
bindest=$(bindir)/$(bin)

all: $(bin)

$(bin): $(src)
	@echo "Building executable"
	$(lisp) --eval "(asdf:oos 'asdf:program-op :$(system))"

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
