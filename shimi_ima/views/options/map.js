function(doc) {
        if (doc.category === 'index')  { emit(doc.name); }
      }