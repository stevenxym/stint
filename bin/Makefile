stintc: stintc.o
	gcc stintc.c -o stintc

.PHONY: clean install uninstall move
clean:
	rm -f stintc.o stintc

install:
	sudo cp stintc /usr/bin/

uninstall:
	sudo rm -f /usr/bin/stintc

move:
	cp stintc ../compiler/
