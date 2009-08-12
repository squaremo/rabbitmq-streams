/*
abstract class Batcher(val count:Float, timeout:Float) {

}

class ByteBasedBatcher(val maxSize:Long,
                       val timeout:Long,
                       var msgs:List[InputMessage]){
  private var totalSize:Long = 0
  private var timeLeft_ms:Long = if ()
  def act() {
    var lastTime:Long
    while (true) {
      lastTime = System.currentTimeMillis
      receiveWithin(timeLeft_ms) {
        case TIMEOUT => ship(msgs)
        case msg : InputMessage => {
            if (dasBootIstVoll msg) {
              timeLeft_ms = timeout
              ship
              state.put("deadline", min(System.currentTimeMillis + timeout, timeout))
              setState(state)}
            timeLeft_ms = max(0.0, System.currentTimeMillis - lastTime)
            else {
              timeLeft_ms = System.currentTimeMillis - lastTime }}}}}
  def dasBootIstVoll(val msg:InputMessage) = {
    totalSize += msg.count + if (includeOverhead) {padBytes size} {0}
    if (totalSize > maxSize) {
      totalSize = 0

      true}
    else {
      false}}
  def ship() {
  }
}

*/
