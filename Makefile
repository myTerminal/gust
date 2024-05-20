SHELL = /bin/sh
OPT_DIR = /opt

ifeq ($(PREFIX),)
	PREFIX := /usr/local
endif
MANPREFIX := $(PREFIX)/share/man

help:
	@echo "Use one of the following options:"
	@echo "install - Installs gust"
	@echo "uninstall - Uninstalls gust"
	@echo "reinstall - Reinstalls gust"
	@echo "update - Updates gust"

crater-get:
	@echo "Setting up Crater for temporary use..."
	git clone https://github.com/crater-space/cli /tmp/crater-cli

primary-deps:
	@echo "Making sure fzf is installed..."
ifneq ($(shell command -v fzf),)
	@echo "fzf found."
else
	@echo "fzf not found!"
	@echo "Attemping to install fzf using Crater..."
	/tmp/crater-cli/crater install fzf
endif
	@echo "Making sure aircrack-ng is installed..."
ifneq ($(shell command -v aircrack-ng),)
	@echo "aircrack-ng found."
else
	@echo "aircrack-ng not found!"
	@echo "Attemping to install aircrack-ng using Crater..."
	/tmp/crater-cli/crater install aircrack-ng
endif
	@echo "Making sure hcxtools is installed..."
ifneq ($(shell command -v hcxpcapngtool),)
	@echo "hcxtools found."
else
	@echo "hcxtools not found!"
	@echo "Attemping to install hcxtools using Crater..."
	/tmp/crater-cli/crater install hcxtools
endif
	@echo "Making sure hcxdumptool is installed..."
ifneq ($(shell command -v hcxdumptool),)
	@echo "hcxdumptool found."
else
	@echo "hcxdumptool not found!"
	@echo "Attemping to install hcxdumptool using Crater..."
	/tmp/crater-cli/crater install hcxdumptool
endif
	@echo "Making sure hashcat is installed..."
ifneq ($(shell command -v hashcat),)
	@echo "hashcat found."
else
	@echo "hashcat not found!"
	@echo "Attemping to install hashcat using Crater..."
	/tmp/crater-cli/crater install hashcat
endif
	@echo "All required dependencies found."

optional-deps:
	@echo "Looking for 'beep'..."
ifneq ($(shell command -v beep),)
	@echo "'beep' found."
else
	@echo "'beep' not found!"
	@echo "Attemping to install 'beep' using Crater..."
	/tmp/crater-cli/crater install beep
endif

crater-remove:
	@echo "Removing Crater..."
	rm -rf /tmp/crater-cli

req: crater-get primary-deps optional-deps crater-remove

place:
	@echo "Installing commands..."
	sudo install ./commands/* $(PREFIX)/bin/
	sudo install ./scripts/* $(PREFIX)/bin/
	sudo mkdir $(OPT_DIR)/gust
	sudo cp -R ./parsers/* $(OPT_DIR)/gust/
	@echo "commands installed."

manpage:
	@echo "Creating manpage..."
	mkdir -p $(MANPREFIX)/man1
	sudo cp ./man/gust*.1 $(MANPREFIX)/man1/
	@echo "Manpage created."

install: req place manpage
	@echo "gust is now installed."

uninstall:
	@echo "Uninstalling gust..."
	sudo rm $(PREFIX)/bin/gust*
	sudo rm $(MANPREFIX)/man1/gust*.1
	sudo rm -rf $(OPT_DIR)/gust
	@echo "gust has been uninstalled."

reinstall: uninstall install

get-latest:
	git pull origin main

update: get-latest reinstall
