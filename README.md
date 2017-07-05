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

4. Put a symlink to `${CONFIG_DIR}/xmonad-x86_64-linux` into a directory that
   is in your `$PATH`, e.g. `$HOME/bin`. For example:

    ```Bash
    cd ~/bin
    ln -s ../.config/xmonad/xmonad-x86_64-linux xmonad
    ```
