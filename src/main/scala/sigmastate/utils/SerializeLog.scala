package sigmastate.utils

import java.io.File
import java.io.FileWriter
import java.util.UUID.randomUUID

object SerializeLog {

  val logFileName: String = "/tmp/foo.bar." + randomUUID.toString()

  val logFile = new File(SerializeLog.logFileName);
  var Depth: Int = 0;

  //true for serialize, false for deserialize
  //true for Start, false for End
  def logPrintf(isStart: Boolean, isSerialize: Boolean, strLog: String) = {
    if (!isStart) Depth -= 1;
    var strSpaces: String = "  " * Depth;
    var strHeaderStartEnd = if (isStart) "[Start]" else "[End]";
    var strHeaderSerialize = if (isSerialize) "[Serialize]" else "[Deserialize]";
    val fr = new FileWriter(logFile, true)
    if (isStart) Depth += 1;

    try {
      fr.write(strSpaces + strHeaderStartEnd + strHeaderSerialize + strLog + "\n");
    }

    finally {
      fr.close()
    }

  }
}