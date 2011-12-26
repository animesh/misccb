#! /bin/sh
SETUP_PRODUCTNAME="google-earth"
SETUP_PRODUCTVER="5.2.1.1329-1"
SETUP_COMPONENTNAME="Default"
SETUP_COMPONENTVER="5.2.1.1329-1"
SETUP_INSTALLPATH="/media/DATA/google-earth/"
SETUP_SYMLINKSPATH="/usr/local/bin"
SETUP_CDROMPATH=""
SETUP_DISTRO="debian"
SETUP_OPTIONTAGS=""
export SETUP_PRODUCTNAME SETUP_PRODUCTVER SETUP_COMPONENTNAME SETUP_COMPONENTVER
export SETUP_INSTALLPATH SETUP_SYMLINKSPATH SETUP_CDROMPATH SETUP_DISTRO SETUP_OPTIONTAGS
#!/bin/sh

#export SETUP_INSTALLPATH="!!INSTALLPATH!!"

if [ "x$SETUP_INSTALLPATH" != "x" ] ; then
    DESKTOPFILE="$SETUP_INSTALLPATH/Google-googleearth.desktop"
    if [ -f "$DESKTOPFILE" ]; then
        echo "Uninstalling desktop menu entries..."
        TMPPATH="$PATH"
        PATH="$PATH:$SETUP_INSTALLPATH/linux/xdg"
        xdg-desktop-menu uninstall "$DESKTOPFILE"
        xdg-desktop-icon uninstall "$DESKTOPFILE"
        PATH="$TMPPATH"
        rm -f "$DESKTOPFILE"
    fi

    MIMEFILE="$SETUP_INSTALLPATH/googleearth-mimetypes.xml"
    if [ -f "$MIMEFILE" ]; then
        echo "Uninstalling mimetypes..."
        TMPPATH="$PATH"
        PATH="$PATH:$SETUP_INSTALLPATH/linux/xdg"
        xdg-mime uninstall "$MIMEFILE"
        PATH="$TMPPATH"
        rm -f "$MIMEFILE"
    fi
fi

# end of preuninstall.sh ...

