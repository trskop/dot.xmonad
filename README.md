# local-xmonad

XMonad with user configuration that uses Stack for compilation.

Tested only on *Debian 9 (stretch)*.


## Installation

1. Install dependencies:

    ```Bash
    apt install libx11-dev libxinerama-dev libxext-dev libxrandr-dev libxft-dev
    ```

2. Clone this repository into `$HOME/.config/xmonad` (XDG style), or into
   `$HOME/.xmonad`. Unfortunatelly current version of XMonad doesn't look for
   build script in XDG compliant directory. For that reason it's a good idea to
   do following:

    ```Bash
    cd
    ln -s .config/xmonad/ .xmonad
    ```

3. Run `${CONFIG_DIR}/build` script.

4. Install vanila `xmonad` into a directory that is in a default `$PATH`, using
   directories like `$HOME/bin` or `$HOME/.local/bin` may not work. Directory
   like `/usr/loca/bin` may be the best place overall.

    If you have rights to write into `/usr/local/bin` directly:

    ```Bash
    stack --local-bin-path=/usr/local/bin install xmonad
    ```

    Otherwise use something like:

    ```Bash
    stack --local-bin-path=. install xmonad
    sudo mv xmonad /usr/local/bin
    sudo chown root:root /usr/local/bin/xmonad
    ```
