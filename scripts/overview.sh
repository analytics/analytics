#!/usr/bin/sh
find src -name "*.hs" -print | xargs graphmod | dot -Tpng > images/overview.png
