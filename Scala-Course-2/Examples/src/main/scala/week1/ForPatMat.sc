import week1._

object exercise {

  val data: List[JSON] = List(
    JObj(Map(
      "firstName" -> JStr("John"),
      "lastName" -> JStr("Smith"),
      "address" -> JObj(Map(
        "streetAddress" -> JStr("21 2nd Street"),
        "state" -> JStr("NY"),
        "postalCode" -> JNum(10021)
      )),
      "phoneNumbers" -> JSeq(List(
        JObj(Map(
          "type" -> JStr("home"),
          "number" -> JStr("212 555-1234")
        )),
        JObj(Map(
          "type" -> JStr("fax"),
          "number" -> JStr("646 5555-4567")
        ))
      ))
    )),
    JObj(Map(
      "firstName" -> JStr("Teddy"),
      "lastName" -> JStr("Roosevelt"),
      "address" -> JObj(Map(
        "streetAddress" -> JStr("32 3rd Street"),
        "state" -> JStr("WSH"),
        "postalCode" -> JNum(30415)
      )),
      "phoneNumbers" -> JSeq(List(
        JObj(Map(
          "type" -> JStr("home"),
          "number" -> JStr("313 555-1234")
        )),
        JObj(Map(
          "type" -> JStr("fax"),
          "number" -> JStr("646 5555-4567")
        ))
      ))
    ))
  )

  for {
    JObj(bindings) <- data
    JSeq(phones) = bindings("phoneNumbers")
    JObj(phone) <- phones
    JStr(digits) = phone("number")
    if digits startsWith "313"
  } yield (bindings("firstName"), bindings("lastName"))

}