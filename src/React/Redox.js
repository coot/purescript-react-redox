"use strict"

exports.unsafeShallowEqual = function(obj1, obj2) {
  if (obj1 !== obj2)
    return false

  var keys1 = Object.getOwnPropertyNames(obj1)
  var keys2Length = Object.getOwnPropertyNames(obj2).length
  if (keys1.length != keys2Length)
    return false

  for (var i=0, len=keys1.length; i < len; i++) {
    var prop = keys1[i];
    if (obj1[prop] != obj2[prop])
      return false
  }
  return true
}
