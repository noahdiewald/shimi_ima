function putDoc(doc) {
  if (!sessionStorage[doc._id]) {
    sessionStorage[doc._id] = JSON.stringify(doc);
  }
  
  return doc._id;
}

function getDoc(docId) {
  var doc = sessionStorage[docId];
  
  if (doc) {
    return JSON.parse(doc);
  } else {
    return null;
  }
}
