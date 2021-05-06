package launchers

import gbge.backend.server.CustomServer
import risk.backend.Risk
import risk.shared.ClientRisk

object StandardLauncher extends CustomServer {
  gbge.shared.RG.registeredGames = List(ClientRisk)
  gbge.backend.RG.registeredGames = List(Risk)

  assert(gbge.backend.RG.registeredGames.size == gbge.shared.RG.registeredGames.size)
  gbge.backend.RG.registeredGames.zip(gbge.shared.RG.registeredGames).foreach(a => {
    assert(a._1.frontendGame == a._2)
  })
}
