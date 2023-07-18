convert_clip <- function() {
  gsub("\\\\", "/", readClipboard())
}
convert_clip()
