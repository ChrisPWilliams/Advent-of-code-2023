package advent.of.code.utils

import advent.of.code.utils
import cats.effect.IO
import org.http4s.{Headers, Method, Request, RequestCookie}
import org.http4s.headers.Cookie
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._

trait Solution {

  val day: Int

  def solve(puzzleInput: String): String

  private val cookie = Cookie(RequestCookie(
    name = "session",
    content = utils.Cookie.value)
  )

  private val request = Request[IO](
    method = Method.GET,
    uri = uri"https://adventofcode.com/2023/day/".addPath(day.toString ++ "/input"),
    headers = Headers(
      cookie
    )
  )

  private def getResult(client: Client[IO]): IO[Unit] = {
    val response = client.expect[String](request)
    response.flatMap(puzzleInput => IO.println(solve(puzzleInput)))
  }

  def run: IO[Unit] = EmberClientBuilder
    .default[IO]
    .build
    .use(client => getResult(client))

}
