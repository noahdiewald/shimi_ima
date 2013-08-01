exports.simple_schema = {
  '_id': 'http://example.com/project-db9b2f5a635078bf094b2a95931d0073/bim',
  'schema':
  {
    '$schema': 'http://json-schema.org/draft-04/schema',
    'title': 'bim',
    'description': 'Kind of like a cold drip.',
    'type': 'object',
    'required': ['_id', 'doctype', 'created_at_', 'created_by_', 'fieldsets'],
    'properties':
    {
      '_id':
      {
        'description': 'The unique identifier for a document',
        'type': 'string'
      },
      '_rev':
      {
        'description': 'The document revision',
        'type': 'string'
      },
      'doctype':
      {
        'description': 'The document doctype',
        'type': 'string',
        'pattern': '^http://example.com/project-db9b2f5a635078bf094b2a95931d0073/bim$'
      },
      'created_at_':
      {
        'description': 'The time and date of document creation',
        'type': 'string',
        'format': 'date-time'
      },
      'created_by_':
      {
        'description': 'The user who created the document',
        'type': 'string'
      },
      'updated_at_':
      {
        'description': 'The time and date of the last document update',
        'type': ['string', 'null'],
        'format': 'date-time'
      },
      'updated_by_':
      {
        'description': 'The user who last updated the document',
        'type': ['string', 'null']
      },
      'deleted_':
      {
        'description': 'Indicates whether the document was deleted',
        'type': 'boolean'
      },
      'fieldsets':
      {
        'description': 'The fieldsets of the document',
        'type': 'array',
        'items': [
          {
            'type': 'object',
            'required': ['id', 'order', 'fields'],
            'properties':
            {
              'title': 'Hip',
              'description': '',
              'id':
              {
                'description': 'Unique identifier for this type of fieldset.',
                'type': 'string',
                'pattern': '^b9ad37ea17a58d9be32160f393770e5d$'
              },
              'fields':
              {
                'description': 'The fields defined for this fieldset.',
                'type': 'array',
                'items': [
                  {
                    'title': 'CalTest',
                    'description': '',
                    'type': 'object',
                    'required': ['id', 'instance', 'value'],
                    'id':
                    {
                      'description': 'Unique identifier for this type of field.',
                      'type': 'string',
                      'pattern': '^25250e2ead108a8f60213f24040007e4$'
                    },
                    'instance':
                    {
                      'description': 'Unique identifier for a field instance.',
                      'type': 'string',
                      'pattern': '^[a-z0-9]{32}$'
                    },
                    'value':
                    {
                      'description': 'date',
                      'type': ['string','null'],
                      'format': 'date-time'
                    }
                  },
                  {
                    'title': 'FF',
                    'description': '',
                    'type': 'object',
                    'required': ['id', 'instance', 'value'],
                    'id':
                    {
                      'description': 'Unique identifier for this type of field.',
                      'type': 'string',
                      'pattern': '^25250e2ead108a8f60213f240400248f$'
                    },
                    'instance':
                    {
                      'description': 'Unique identifier for a field instance.',
                      'type': 'string',
                      'pattern': '^[a-z0-9]{32}$'
                    },
                    'value':
                    {
                      'description': 'integer',
                      'type': ['integer', 'string', 'null'],
                      'pattern': '^$',
                      'maximum': 10,
                      'minimum': 0
                    }
                  },
                  {
                    'title': 'Yer',
                    'description': '',
                    'type': 'object',
                    'required': ['id', 'instance', 'value'],
                    'id':
                    {
                      'description': 'Unique identifier for this type of field.',
                      'type': 'string',
                      'pattern': '^b9ad37ea17a58d9be32160f393771cdd$'
                    },
                    'instance':
                    {
                      'description': 'Unique identifier for a field instance.',
                      'type': 'string',
                      'pattern': '^[a-z0-9]{32}$'
                    },
                    'value':
                    {
                      'description': 'integer',
                      'type': ['boolean', 'null']
                    }
                  }
                ]
              }
            }
          }
        ]
      }
    }
  }
};