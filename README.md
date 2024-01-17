# gust

[![License](https://img.shields.io/github/license/myTerminal/gust.svg)](https://opensource.org/licenses/MIT)  
[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y5E5GL7)

An experimental CLI tool for penetration testing

> **Caution:** This project is intended solely for penetration testing purposes on your own networks. Unauthorized use on systems or networks that do not belong to you is strictly prohibited and may be illegal. I do not endorse, support, or bear any responsibility for any illegal activities conducted using this project. It is essential to adhere to ethical hacking practices and obtain proper authorization before engaging in penetration testing activities. Unauthorized penetration testing on external networks may lead to severe legal consequences. Use this tool responsibly and within the bounds of the law.

## Installation

There are a few different ways to get *gust*.

### Compile from source

    # Clone project to the local workspace
    git clone https://github.com/myTerminal/gust.git

    # Switch to the project directory
    cd gust

    # Install with `make`
    make install

### Automatic installation

Simply execute the below command in a terminal and the rest should be automatic.

    /bin/bash -c "$(curl https://raw.githubusercontent.com/myTerminal/gust/main/install)"

### Through a package manager

*gust* will soon be available to install from your operating system's package manager.

## How to Use

Currently, *gust* only supports a single command:

1. **gust-pmkid+** - runs a PMKID attack through prompts.

### Further help with commands

To learn more about the usage of a particular command, refer to the corresponding `manpage`:

    man gust-pmkid+

## Updating

In order to update *gust*, simply run:

    gust-update

## Uninstalling

In order to uninstall *gust*, simply run:

    gust-uninstall

## External Dependencies

*gust* depends on a few very basic and popular tools:

 - [aircrack-ng](https://www.aircrack-ng.org)
 - [hcxtools](https://github.com/ZerBea/hcxtools)
 - [hcxdumptool](https://github.com/ZerBea/hcxdumptool)
 - [hashcat](https://hashcat.net/hashcat)

## Optional External Dependencies

Currently, *gust* only depends on a single optional dependency. It is attempted to be fetched during the installation:

 - [beep](https://pkgs.org/search/?q=beep)

If not installed automatically, you may install it manually.

## To-Do's

- Use audio feedback
- Reduce manual copy/paste
