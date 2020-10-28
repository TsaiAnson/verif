package verif

import freechips.rocketchip.config.Config

class VerifConfig extends Config(
  // This should set up the edges for TileVisiblityNodeKey...
  new verif.WithNVerifTiles ++
  new chipyard.config.AbstractConfig
)

class EmptyVerifConfig extends Config(
  // This should set up the edges for TileVisiblityNodeKey...
  new chipyard.config.AbstractConfig
)