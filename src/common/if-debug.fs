module Aornota.BridgeSim.Common.IfDebug

let ifDebug (debug:'a) (notDebug:'a) =
#if DEBUG
    debug
#else
    notDebug
#endif
