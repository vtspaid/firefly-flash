

InputFileUI <- function(id) {
  fileInput(NS(id, "inputfile"), "Choose a .wav or .mp3 file",
            accept = c(".wav", ".mp3"))
}

InputFileServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
    print("starting infile server")
      infile <- input$inputfile
      print(infile)
      if(grepl('.*\\.wav', ignore.case=TRUE, infile$datapath)) {
        audio <- readWave(infile$datapath)
      } else {
        audio <- readMP3(infile$datapath)
      }
      audio <- Wave(left = audio@left[seq(1, length(audio@left), by = 4)],
      samp.rate = audio@samp.rate / 4, bit = 16)
      data <- list(file = input$inputfile$datapath,
                audio = audio)
      print("ending infile server")
      str(data)
      names(data)
      data
      })
  })
}