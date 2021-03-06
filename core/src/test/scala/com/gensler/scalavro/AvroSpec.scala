package com.gensler.scalavro.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import spray.json.{ JsValue, PrettyPrinter }
import com.gensler.scalavro.util.Logging

trait AvroSpec extends FlatSpec with Matchers with Logging {

  protected def prettyPrint(json: JsValue) {
    val buff = new java.lang.StringBuilder
    PrettyPrinter.print(json, buff)
    log debug buff.toString
  }

}
