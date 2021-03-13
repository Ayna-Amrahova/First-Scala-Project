package com.lightbend.training.scalatrain

import scala.collection.immutable.Seq

case class Train(info: TrainInfo, schedule: Seq[(Time, Station)]){

  require(schedule.take(2).size == 2, "Schedule must contain at least 2 elements.")

//  val stations: Seq[Station] = schedule.map(tuple => tuple._2) same with line10
  val stations: Seq[Station] = schedule.map(_._2)

  def timeAt(station: Station): Option[Time] =
  //solution 1 is:
  //  schedule.find(timeAndStation => timeAndStation._2 == station).map(timeAndStation => timeAndStation._1)

  //solution 2 is:
  schedule.find{
    case (_, needle) => needle == station
  }.map{
    case (time, _) => time
  }

}
