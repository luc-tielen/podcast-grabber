
build:
	stack build

clean:
	stack clean

install:
	stack install
	@echo 'Do not forget to add ~/.local/bin/ to your $$PATH'

.PHONY: build clean install

