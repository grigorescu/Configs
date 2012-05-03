#!/usr/bin/env python2.7

import dbus, sys, os

try:
    bus = dbus.SessionBus()
    remote_object = bus.get_object("org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2")
    props = dbus.Interface(remote_object, "org.freedesktop.DBus.Properties")
    metadata = props.Get("org.mpris.MediaPlayer2.Player", "Metadata")
    print u"%s %s - %s %s" %("-=", metadata['xesam:title'], metadata['xesam:artist'][0], "=-")
except:
    print " "
    sys.exit(0)

