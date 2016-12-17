package problems

import java.security.MessageDigest
import sun.security.provider.MD5

object Day5 {

  def main(args: Array[String]): Unit = {
    val input: String = "ojvtpuvg"
    part1(input)
    part2(input)
  }

  def part1(input: String) = {
    var password: String = ""
    var index: Int = 0
    var messageDigest: MessageDigest = MessageDigest.getInstance("MD5")
    var inputBytes: Array[Byte] = input.getBytes

    while (password.length() < 8) {
      var hashInput: Array[Byte] = inputBytes ++ index.toString.getBytes
      var hash: String = bytesToHex(messageDigest.digest(hashInput))
      if (hash.startsWith("00000")) {
        password += hash(5).toLower
      }
      index += 1
    }
    println(password)
  }

  def part2(input: String) = {
    var password: Array[Char] = ("_" * 8).toCharArray
    var index: Int = 0
    var messageDigest: MessageDigest = MessageDigest.getInstance("MD5")
    var inputBytes: Array[Byte] = input.getBytes

    while (password.contains('_')) {
      var hashInput: Array[Byte] = inputBytes ++ index.toString.getBytes
      var hash: String = bytesToHex(messageDigest.digest(hashInput))
      if (hash.startsWith("00000")) {
        var position: Int = hash(5).asDigit
        if (position < 8 && password(position) == '_') {
          password(position) = hash(6).toLower
        }
      }
      index += 1
    }
    println(password.mkString)
  }

  private def bytesToHex(bytes: Array[Byte]): String = {
    val hexArray: Array[Char] = "0123456789ABCDEF".toCharArray()
    var hexChars: Array[Char] = new Array[Char](bytes.length * 2)
    for (i <- 0 until bytes.length) {
      var v: Int = bytes(i) & 0xFF
      hexChars(i * 2) = hexArray(v >>> 4)
      hexChars(i * 2 + 1) = hexArray(v & 0x0F)
    }
    return hexChars.mkString
  }

}
