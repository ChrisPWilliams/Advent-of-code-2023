package advent.of.code.day5

import advent.of.code.utils.Solution
import cats.effect.IOApp


object DayFive extends Solution with IOApp.Simple {

  lazy val day = 5

  def solve(puzzleInput: String): String = {
    val partOne = solvePartOne(puzzleInput)
    val partTwo = ""//solvePartTwo(puzzleInput)
    "Part 1 solution: " ++ partOne ++ "\nPart 2 solution: " ++ partTwo
  }

  case class MappingSection(
                       destinationStart: BigInt,
                       sourceStart: BigInt,
                       rangeLength: BigInt
                       )

  case class Almanac(
                    seeds: Seq[BigInt],
                    seedToSoil: BigInt => BigInt,
                    soilToFertilizer: BigInt => BigInt,
                    fertilizerToWater: BigInt => BigInt,
                    waterToLight: BigInt => BigInt,
                    lightToTemperature: BigInt => BigInt,
                    temperatureToHumidity: BigInt => BigInt,
                    humidityToLocation: BigInt => BigInt
                    )

  def parseInput(input: String, puzzlePart: Int): Almanac = {
    val getSectionsPattern =
      """seeds: ([0-9 ]+)\n
        |seed-to-soil map:\n([0-9 \n]+)\n
        |soil-to-fertilizer map:\n([0-9 \n]+)\n
        |fertilizer-to-water map:\n([0-9 \n]+)\n
        |water-to-light map:\n([0-9 \n]+)\n
        |light-to-temperature map:\n([0-9 \n]+)\n
        |temperature-to-humidity map:\n([0-9 \n]+)\n
        |humidity-to-location map:\n([0-9 \n]+)""".stripMargin.r
    input match {
      case getSectionsPattern(seeds, seed2Soil, soil2Fert, fert2Water, water2Light, light2Temp, temp2Humid, humid2Loc) =>
        Almanac(
          seeds = seeds.split(" ").map(BigInt(_)),
          seedToSoil = parseMappingSectionToFunc(seed2Soil),
          soilToFertilizer = parseMappingSectionToFunc(soil2Fert),
          fertilizerToWater = parseMappingSectionToFunc(fert2Water),
          waterToLight = parseMappingSectionToFunc(water2Light),
          lightToTemperature = parseMappingSectionToFunc(light2Temp),
          temperatureToHumidity = parseMappingSectionToFunc(temp2Humid),
          humidityToLocation = parseMappingSectionToFunc(humid2Loc)
        )
    }
  }

  val parseMappingSectionToRaw: String => Seq[MappingSection] = { section =>
    val getValuesPattern = "([0-9]+) ([0-9]+) ([0-9]+)".r
    section.split("\n").map {
      case getValuesPattern(destinationStart, sourceStart, rangeLength) =>
        MappingSection(BigInt(destinationStart), BigInt(sourceStart), BigInt(rangeLength))
    }
  }

  val mappingsToFunc: Seq[MappingSection] => BigInt => BigInt = { mappings =>
    val adders = mappings.map(mappingToAdder)
    int => adders.flatMap(adder => adder(int)).headOption.getOrElse(int)
  }

  def mappingToAdder(mapping: MappingSection): BigInt => Option[BigInt] = {
    int: BigInt => if (int >= mapping.sourceStart && int < (mapping.sourceStart + mapping.rangeLength)) {
      Some(int + (mapping.destinationStart - mapping.sourceStart))
    } else None
  }

  val parseMappingSectionToFunc: String => BigInt => BigInt = parseMappingSectionToRaw andThen mappingsToFunc

  def part2GetRanges(seeds: String): Seq[BigInt] = {
    val parseRangePattern = "([0-9]+) ([0-9]+)".r
    parseRangePattern.findAllMatchIn(seeds).flatMap(m =>
      BigInt(m.group(1)) to (BigInt(m.group(1)) + BigInt(m.group(2)) - 1)
    ).toSeq //might need the regex but otherwise got to scrap this approach
  }

  def solvePartOne(puzzleInput: String): String = {
    val almanac = parseInput(puzzleInput, puzzlePart = 1)
    val getSeedLocation: BigInt => BigInt = {
      almanac.seedToSoil andThen
        almanac.soilToFertilizer andThen
        almanac.fertilizerToWater andThen
        almanac.waterToLight andThen
        almanac.lightToTemperature andThen
        almanac.temperatureToHumidity andThen
        almanac.humidityToLocation
    }
    almanac.seeds.map(getSeedLocation).min.toString
  }
  // part 2 plan: work backwards introducing concept of seed streams: each unique path through the maps is one stream.
  // each stream hits a contiguous block of locations- within a stream, seed x + 1 is guaranteed to be at location f(x) + 1
  // check each block from lowest upwards, mapping back to the seed ranges, checking if we have a seed in the range and getting the lowest

}
