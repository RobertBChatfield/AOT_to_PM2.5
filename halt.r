halt = function(message='') {
# Stop execution without issuing an error message
  cat('stopped',message,'\n')
  capture.output(stop(),type='message')
}
