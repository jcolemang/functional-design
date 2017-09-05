
module FileWatcherTypes where

import Path

type ProcessRunner a = Path Rel File -> IO a
