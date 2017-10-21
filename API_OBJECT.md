# Object

## List

Returns a list of objects associated with the specified bucket.

**URI**

```
GET /buckets/${BUCKET}/objects
```

**URI parameters**

Name   | Type   | Default    | Description
------ | ------ | ---------- | ------------------
BUCKET | string | _required_ | Name of the bucket

**Request parameters**

Name      | Type   | Default    | Description
--------- | ------ | ---------- | ------------------
rows      | int    | 100        | Sets the maximum number of objects returned in the response body
lexprefix | string | -          | Limits the response to keys that begins with the specified prefix
lexmarker | string | -          | Limits the response to keys that starts from the specified marker

**Response**

List of objects

**Example**

```bash
curl -fsSL \
    -XGET ${ENDPOINT}/buckets/example-bucket/objects \
    -H "authorization: Bearer ${ACCESS_TOKEN}"
 
[
  {
    "data": {
      "size": 1
    },
    "id": "example1"
  },
  {
    "data": {
      "size": 2
    },
    "id": "example2"
  }
```



## Read

Returns an object associated with the specified bucket.

**URI**

```
GET /buckets/${BUCKET}/objects/${KEY}
```

**URI parameters**

Name   | Type   | Default    | Description
------ | ------ | ---------- | ------------------
BUCKET | string | _required_ | Name of the bucket
KEY    | string | _required_ | Key of the object

**Response**

Object

**Example**

```bash
curl -fsSL \
    -XGET ${ENDPOINT}/buckets/example-bucket/objects/example \
    -H "authorization: Bearer ${ACCESS_TOKEN}"
```



## Update

Creates new or updates an existing object.

**URI**

```
PUT /buckets/${BUCKET}/objects/${KEY}
```

**URI parameters**

Name   | Type   | Default    | Description
------ | ------ | ---------- | ------------------
BUCKET | string | _required_ | Name of the bucket
KEY    | string | _required_ | Key of the object

**URI parameters**

Name             | Type     | Default    | Description
---------------- | -------- | ---------- | ------------------
content-type     | string   | _required_ | MIME type of the object
content-length   | string   | _required_ | The size of the object in bytes
content-encoding | string   | -          | Indicates which encodings were applied to the object
cache-control    | string	  | -          | Caching directives (`no-store`, `no-cache`, `max-age=31536000`, etc.)
x-datastore-acl  | [object] | -          | ACL entries associated with the object

**Response**

empty

**Example**

```bash
curl -fsSL \
    -XPUT ${ENDPOINT}/buckets/example-bucket/objects/example \
    -H "authorization: Bearer ${ACCESS_TOKEN}" \
    -H 'x-datastore-acl: [{"id":"anonymous","data":{"access":"r-"}}]' \
    -H 'Content-Type: text/plain' \
    --data-binary @example.txt
```



## Delete

Removes an existing object.

**URI**

```
DELETE /buckets/${BUCKET}/objects/${KEY}
```

**URI parameters**

Name   | Type   | Default    | Description
------ | ------ | ---------- | ------------------
BUCKET | string | _required_ | Name of the bucket
KEY    | string | _required_ | Key of the object

**Response**

empty

**Example**

```bash
curl -fsSL \
    -XDELETE ${ENDPOINT}/buckets/example-bucket/objects/example \
    -H "Authorization: Bearer ${ACCESS_TOKEN}"
```