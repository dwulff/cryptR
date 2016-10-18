#' Encrypt data
#'
#' \code{encrypt} is a simplified wrapper for the encryption of R data
#' objects using the \code{sodium} package
#'
#' @param x any data object.
#' @param password a string specifying the passphrase to en- and decrypt
#'   the data.
#'
#' @return encrypted data
#'
#' @seealso \code{\link{decrypt}}
#'
#' @export

encrypt = function(x,password){
  key    = sodium:::sha256(charToRaw(password))
  msg    = serialize(x,NULL)
  cipher = sodium:::data_encrypt(msg, key)
  return(cipher)
  }

#' Decrypt data
#'
#' \code{decrypt} is a simplified wrapper for the decryption of R data
#' objects using the \code{sodium} package
#'
#' @param x any data object.
#' @param password a string specifying the passphrase to en- and decrypt
#'   the data.
#'
#' @return encrypted data
#'
#' @seealso \code{\link{encrypt}}
#'
#' @export

decrypt = function(x,password){
  key  = sodium:::sha256(charToRaw(password))
  orig = sodium:::data_decrypt(x,key)
  orig = unserialize(orig)
  return(orig)
  }
