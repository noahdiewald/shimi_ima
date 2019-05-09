exports.validate_doc_update = function (newDoc, saveDoc, userCtx, testEnv) {
  'use strict';

  // This is a workaround so that I can easily run unit tests
  if (testEnv !== true) {
    testEnv = false;
  }

  // A predicate function to detect blank strings.
  // Warning: this is a bad implementation.
  var isBlank = function (value) {
    return (((/^\s*$/).test(value)) || (value === null) || (value === undefined) || (typeof value === 'number' && isNaN(value)) || (Object.prototype.toString.call(value) === '[object Array]' && value.length === 0));
  };

  var forbid = function (name, msg) {
    var docid;

    if (newDoc._id) {
      docid = newDoc._id;
    } else {
      docid = 'No ID';
    }

    var errorMsg = JSON.stringify({
      fieldname: name,
      message: msg,
      docid: docid
    });

    // This is used when running unit tests
    if (testEnv) {
      throw new Error(errorMsg);
    } else {
      throw ({
        forbidden: errorMsg
      });
    }
  };

  var charseq = function (newDoc, saveDoc, userCtx) {
    if (newDoc.category === 'charseq') {
      if (isBlank(newDoc.name)) {
        forbid('Name', 'must be filled in.');
      }

      [
        [newDoc.characters, 'Characters'],
        [newDoc.sort_ignore, 'Ignore'],
        [newDoc.vowels, 'Vowels'],
        [newDoc.consonants, 'Consonants']
      ].forEach(function (item) {
        if (item[0] === false) {
          forbid(item[1], 'contains quoting errors.');
        }
      });
    }
  };

  var not_parens = function (parens) {
    return (parens !== 'open' && parens !== 'close' && parens !== 'exopen' && parens !== 'exclose');
  };

  var validate_paren_count = function (pcount, expcount, index) {
    if (pcount < 0 || expcount < 0) {
      forbid('Condition ' + index, 'too many closing parenthesis.');
    }
  };

  var validate_exparen_nesting = function (expcount, index) {
    if (expcount > 1) {
      forbid('Condition ' + index, 'existential parenthesis cannot be nexted.');
    }
  };

  var validate_exparen_fieldset = function (exfs, fieldset, is_or, index) {
    if (exfs !== fieldset && !is_or) {
      forbid('Condition ' + index, 'all conditions within existential scope must have ' + 'the same fieldset.');
    }
  };

  var validate_negate = function (negate, index) {
    if (negate !== true && negate !== false) {
      forbid('Condition ' + index, 'negate must be true or false');
    }
  };

  var validate_field = function (field, index) {
    if (isBlank(field) || !(/^\w*$/).test(field)) {
      forbid('Condition ' + index, 'invalid field.');
    }
  };

  var validate_fieldset = function (fieldset, index) {
    if (isBlank(fieldset) || !(/^\w*$/).test(fieldset)) {
      forbid('Condition ' + index, 'invalid fieldset.');
    }
  };

  var validate_operator = function (operator, argument, index) {
    switch (operator) {
    case 'hasGreater':
    case 'hasLess':
    case 'hasExactly':
      if (typeof argument !== 'number' || (argument % 1) > 0) {
        forbid('Condition ' + index, 'integer argument required.');
      }
      break;
    case 'equal':
    case 'match':
    case 'greater':
    case 'less':
    case 'match':
    case 'member':
      if (typeof argument === 'string' && isBlank(argument)) {
        forbid('Condition ' + index, 'blank argument is not allowed.');
      }
      break;
    case 'true':
    case 'blank':
    case 'isDefined':
      if (!isBlank(argument)) {
        forbid('Condition ' + index, 'no argument allowed for this condition.');
      }
      break;
    default:
      forbid('Condition ' + index, 'invalid operator.');
    }
  };

  var documents = function (newDoc, saveDoc, userCtx) {
    // A more detailed error message function for cases where we
    // want to indication what field caused the problem.
    var forbidField = function (field, msg) {
      var errorMsg = JSON.stringify({
        fieldname: field.name,
        instance: field.instance,
        message: msg
      });

      if (testEnv) {
        throw new Error(errorMsg);
      } else {
        throw ({
          forbidden: errorMsg
        });
      }
    };

    // Ensure that the date is formatted according to the standard of
    // this application.
    var dateFormat = function (field) {
      var pattern = (/^\d{4}-\d{2}-\d{2}$/);

      if (!pattern.test(field.value) && isNaN(Date.parse(field.value))) {
        forbidField(field, 'date must be in format yyyy-mm-dd');
      }
    };

    var myDateToUTC = function (myDate) {

    };

    // See if a date is earlier than a maximum date. If it isn't fail.
    var isEarlier = function (field) {
      if (field.value >= field.max) {
        var message = 'date must be earlier than or equal to ' + field.max + '.';
        forbidField(field, message);
      }
    };

    // See if a date is later than a minimum date. If it isn't fail.
    var isLater = function (field) {
      if (field.value < field.min) {
        var message = 'date must be later than ' + field.min + '.';
        forbidField(field, message);
      }
    };

    // See if a date should be today. If it isn't fail.
    var isToday = function (field) {
      if (field.value !== new Date().toLocaleFormat('%Y-%m-%d')) {
        forbidField(field, 'date must be today.');
      }
    };

    // See if a date should be in the future. If it isn't fail.
    var isFuture = function (field) {
      if (field.value <= new Date().toLocaleFormat('%Y-%m-%d')) {
        forbidField(field, 'date must be in the future.');
      }
    };

    // See if a date should be in the past. If it isn't fail.
    var isPast = function (field) {
      if (field.value > new Date().toLocaleFormat('%Y-%m-%d')) {
        forbidField(field, 'date must be in the past.');
      }
    };

    // Make sure that the date is not outside of a specified range
    var dateRange = function (field) {
      var pattern = (/^\d{4}-\d{2}-\d{2}$/);

      if (pattern.test(field.max)) {
        isEarlier(field);
      }
      if (pattern.test(field.min)) {
        isLater(field);
      }
      if (field.min === 'today' && field.max === 'today') {
        isToday(field);
      } else {
        if (field.min === 'today') {
          isFuture(field);
        }
        if (field.max === 'today') {
          isPast(field);
        }
      }
    };

    // Test if it is a number and not NaN.
    var isNumber = function (value) {
      return ((typeof value === 'number') && !(isNaN(value)));
    };

    var isInteger = function (field) {
      var message = 'Expected an integer but got rational number';

      if (field.value % 1 !== 0) {
        forbidField(field, message);
      }
    };

    // Make sure this is a valid number
    var isValidNumber = function (field) {
      if (!isNumber(field.value)) {
        forbidField(field, 'Not a valid number');
      }
    };

    // Determine if the number is within a given range
    var numberRange = function (field) {
      if (isNumber(field.max) && (field.value > field.max)) {
        forbidField(field, 'Must be less than or equal to ' + field.max);
      }
      if (isNumber(field.min) && (field.value < field.min)) {
        forbidField(field, 'Must be greater than or equal to ' + field.min);
      }
    };

    // Determine if string should match pattern
    var isMatch = function (field) {
      if (!isBlank(field.regex)) {
        var re = new RegExp(field.regex);

        if (!re.test(field.value)) {
          forbidField(field, 'Must match ' + field.regex);
        }
      }
    };

    // Determine if it is an array of strings
    var isStringArray = function (field) {
      if (Object.prototype.toString.call(field.value) !== '[object Array]') {
        forbidField(field, 'Must be an array of strings.');
      }

      field.value.forEach(function (v) {
        if (typeof v !== 'string') {
          forbidField(field, 'Must contain only text.');
        }
      });
    };

    // Determine if it is a string
    var isString = function (field) {
      if (typeof field.value !== 'string') {
        forbidField(field, 'Must be text.');
      }
    };

    // Fail if not a boolean
    var isBoolean = function (field) {
      if (typeof field.value !== 'boolean') {
        forbidField(field, 'Must be true or false.');
      }
    };

    // Fail if neither a boolean nor null
    var isOpenboolean = function (field) {
      if (typeof field.value !== 'boolean' && field.value !== null) {
        forbidField(field, 'Must be true, false or blank.');
      }
    };

    // The following are validation tests that are run for
    // each field by an expression below.
    var requiredField = function (field) {
      if (field.required === true) {
        if (isBlank(field.value)) {
          forbidField(field, 'cannot be blank.');
        }
      }
    };

    // Determine if this is a datefield and run the date validations
    // if it is not blank
    var dateField = function (field) {
      if (field.subcategory === 'date' && !isBlank(field.value)) {
        dateFormat(field);
        dateRange(field);
      }
    };

    // Determine if this is an integer field and the integer validations
    var integerField = function (field) {
      if (field.subcategory === 'integer' && field.value !== '') {
        isValidNumber(field);
        isInteger(field);
        numberRange(field);
      }
    };

    // Determine if this is an rational field and the rational validations
    var rationalField = function (field) {
      if (field.subcategory === 'rational' && field.value !== '') {
        isValidNumber(field);
        numberRange(field);
      }
    };

    // Determine if this is a text field and run appropriate validations
    var textField = function (field) {
      if (field.subcategory === 'text' && !isBlank(field.value)) {
        isString(field);
        isMatch(field);
      }
    };

    // Determine if this is a textarea field and run appropriate validations
    var textareaField = function (field) {
      if (field.subcategory === 'textarea' && !isBlank(field.value)) {
        isString(field);
        isMatch(field);
      }
    };

    // Determine if this is a boolean field make sure it is true or false
    var booleanField = function (field) {
      if (field.subcategory === 'boolean') {
        isBoolean(field);
      }
    };

    // Determine if this is a openboolean field make sure it is true or false
    var openbooleanField = function (field) {
      if (field.subcategory === 'openboolean') {
        isOpenboolean(field);
      }
    };

    // Determine if this is a select field and do appropriate validations
    // NOTE validations on whether a selection is in the list of allowed
    //      values is done in the application layer in the document
    //      resource.
    var selectField = function (field) {
      if (field.subcategory === 'select' && !isBlank(field.value)) {
        isString(field);
      }
    };

    // Determine if this is a docselect field and do appropriate validations
    // NOTE validations on whether a selection is in the list of allowed
    //      values is done in the application layer in the document
    //      resource.
    var docselectField = function (field) {
      if (field.subcategory === 'docselect' && !isBlank(field.value)) {
        isString(field);
      }
    };

    // Determine if this is a docmultiselect field and do appropriate validations
    // NOTE validations on whether a selection is in the list of allowed
    //      values is done in the application layer in the document
    //      resource.
    var docmultiselectField = function (field) {
      if (field.subcategory === 'docmultiselect' && !isBlank(field.value)) {
        isStringArray(field);
      }
    };

    // Determine if this is a multiselect field and do appropriate validations
    // NOTE validations on whether a selection is in the list of allowed
    //      values is done in the application layer in the document
    //      resource.
    var multiselectField = function (field) {
      if (field.subcategory === 'multiselect' && !isBlank(field.value)) {
        isStringArray(field);
      }
    };

    // This is a list of the validation tests to run on each
    // field.
    var validationTests = [requiredField, dateField, booleanField, docmultiselectField, docselectField, integerField, multiselectField, openbooleanField, rationalField, selectField, textField, textareaField];

    // This iterates through the fields and runs the above
    // validation test for each field.
    newDoc.fieldsets.forEach(function (fieldset) {
      if (fieldset.multiple) {
        fieldset.multifields.forEach(function (multifield) {
          multifield.fields.forEach(function (field) {
            validationTests.forEach(function (vTest) {
              vTest(field);
            });
          });
        });
      } else {
        fieldset.fields.forEach(function (field) {
          validationTests.forEach(function (vTest) {
            vTest(field);
          });
        });
      }
    });
  };

  var userIndex = function (newDoc, saveDoc, userCtx) {
    [newDoc.doctype, newDoc.name].forEach(

      function (field) {
        if (isBlank(field)) {
          forbid('All fields', 'must be filled in.');
        }
      });

    if (newDoc.fields.length === 0 || newDoc.fieldLabel === 0) {
      forbid('All fields', 'must be filled in.');
    }

    if (typeof newDoc.show_deleted !== 'boolean') {
      forbid('Show deleted', 'must be true or false');
    }

    if (!(/^\w*$/).test(newDoc.name)) {
      forbid('Name', 'may only contain alphanumeric characters and underscores.');
    }

    if (newDoc.conditions && newDoc.conditions.length > 0) {
      var firstCondition = newDoc.conditions[0];
      var lastCondition = newDoc.conditions[newDoc.conditions.length - 1];
      var parenCount = 0;
      var exParenCount = 0;
      var exParenFS = false;

      if (firstCondition.is_or === true || lastCondition.is_or === true) {
        forbid('Conditions', 'may not begin or end with OR.');
      }

      if (firstCondition.parens === 'close' || firstCondition.parens === 'exclose' || lastCondition.parens === 'exopen' || lastCondition.parens === 'open') {
        forbid('Conditions', 'begin or end with improper parenthesis.');
      }

      newDoc.conditions.forEach(function (condition, index) {
        var is_or = condition.is_or;
        var negate = condition.negate;
        var argument = condition.argument;
        var operator = condition.operator;
        var field = condition.field;
        var fieldset = condition.fieldset;
        var parens = condition.parens;

        validate_paren_count(parenCount, exParenCount, index);
        validate_exparen_nesting(exParenCount, index);

        if (is_or !== true && not_parens(parens)) {
          validate_negate(negate, index);
          validate_field(field, index);
          validate_fieldset(fieldset, index);
          validate_operator(operator, argument, index);
        }

        if (parens === 'open') {
          parenCount = parenCount + 1;
        } else if (parens === 'close') {
          parenCount = parenCount - 1;
        } else if (parens === 'exopen') {
          exParenCount = exParenCount + 1;
        } else if (parens === 'exclose') {
          exParenCount = exParenCount - 1;
          exParenFS = false;
        }

        if (!parens && exParenCount === 1) {
          if (exParenFS) {
            validate_exparen_fieldset(exParenFS, fieldset, is_or, index);
          } else {
            exParenFS = fieldset;
          }
        }
      });

      if (parenCount !== 0 || exParenCount !== 0) {
        forbid('Condition', 'unclosed parenthesis.');
      }
    }
  };

  // readonly users cannot write a document
  if (userCtx.roles.indexOf('readonly') !== -1) {
    forbid(userCtx.name, 'is a read only user.');
  }

  if (newDoc.category === 'charseq') {
    charseq(newDoc, saveDoc, userCtx);
    return 'ok';
  } else if (newDoc.category === 'index') {
    userIndex(newDoc, saveDoc, userCtx);
    return 'ok';
  } else if (!newDoc.category && !!newDoc.doctype && !newDoc.deleted_) {
    documents(newDoc, saveDoc, userCtx);
    return 'ok';
  }

  return 'skipped';
};
