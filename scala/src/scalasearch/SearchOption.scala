package scalasearch

class SearchOption(val shortarg: String, val longarg: String, val func: Function, val desc: String) {
    override def toString() = {
        "SearchOption(shortarg: " + shortarg + ", longarg: " + longarg + ")"
    }
}
