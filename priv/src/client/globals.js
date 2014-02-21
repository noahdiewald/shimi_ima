// # Globals object
//
// A place to temporarily store global objects. Sometimes this is more
// convenient than using other types of client side storage. It is used
// rarely and explicitly using this object.
//
// This is not loaded as a module like other code here. It is concatenated
// to the beginning of the target JavaScript file created by the build
// process.

var globals = {};
