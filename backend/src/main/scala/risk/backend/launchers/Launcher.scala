package risk.backend.launchers

import gbge.backend.config.GameConfig
import gbge.backend.{BackendGameProps, GenericLauncher}
import risk.backend.BackendRiskProps
import zio.{
  ConfigProvider,
  Runtime,
  Scope,
  ZIO,
  ZIOAppArgs,
  ZIOAppDefault,
  ZLayer
}

object Launcher extends ZIOAppDefault {

  private val games: Seq[BackendGameProps[?, ?]] = Seq(BackendRiskProps)

  private val gl = GenericLauncher(games)

  override def run: ZIO[Scope & ZIOAppArgs, Any, Unit] = for {
    args <- ZIO.serviceWith[ZIOAppArgs](_.getArgs)
    config <- GameConfig.resolveConfig
    _ <- ZIO.log(s"The resolved config is: [$config]")
    _ <- gl.launch.provideSomeEnvironment[Scope](scope =>
      scope.add(config).add(args.headOption)
    )
  } yield ()

  override val bootstrap: ZLayer[Any, Nothing, Unit] =
    Runtime.setConfigProvider(ConfigProvider.propsProvider)
}
