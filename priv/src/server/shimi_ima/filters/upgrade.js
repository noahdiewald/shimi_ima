function filter(doc, req) {
  if (doc._id === "_design/shimi_ima") {
    return true;
  } else {
    return false;
  }
}