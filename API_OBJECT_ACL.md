# Object ACL

## List

Returns a list of ACL entries of the bucket/object.

**URI**

```
GET /buckets/${BUCKET}/acl
GET /buckets/${BUCKET}/objects/${KEY}/acl
```

**URI parameters**

Name   | Type   | Default    | Description
------ | ------ | ---------- | ------------------
BUCKET | string | _required_ | Name of the bucket
KEY    | string | _required_ | Key of the object

**Response**

List of ACL entries of the bucket/object

**Example**

```bash
curl -fsSL \
    -XGET ${ENDPOINT}/buckets/example-bucket/objects/example/acl \
    -H "Authorization: Bearer ${ACCESS_TOKEN}"
 
[{"id":"customer","data":{"access":"r-","exp":1508587911}},{"id":"anonymous","data":{"access":"r-"}}]
```



## Update (multiple ACL)

Creates or updates a list of ACL entries of the bucket/object.

**URI**

```
POST /buckets/${BUCKET}/acl
POST /buckets/${BUCKET}/objects/${KEY}/acl
```

**URI parameters**

Name   | Type   | Default    | Description
------ | ------ | ---------- | ------------------
BUCKET | string | _required_ | Name of the bucket
KEY    | string | _required_ | Key of the object

**Payload**

ACL entries

**Response**

Modified list of ACL entries of the bucket/object

**Example**

```bash
curl -fsSL \
    -XPOST ${ENDPOINT}/buckets/example-bucket/objects/example/acl \
    -H "Authorization: Bearer ${ACCESS_TOKEN}" \
    -H 'Content-Type: application/json' \
    -d '[{"id":"anonymous","data":{"access":"r-"}}]'
 
[{"id":"customer","data":{"access":"r-","exp":1508587911}},{"id":"anonymous","data":{"access":"r-"}}]
```



## Read

Returns an ACL entry of the bucket/object.

**URI**

```
GET /buckets/${BUCKET}/acl/${ACL_GROUP}
GET /buckets/${BUCKET}/objects/${KEY}/acl/${ACL_GROUP}
```

**URI parameters**

Name      | Type   | Default    | Description
--------- | ------ | ---------- | ------------------
BUCKET    | string | _required_ | Name of the bucket
KEY       | string | _required_ | Key of the object
ACL_GROUP | string | _required_ | Name of the ACL group

**Response**

ACL entry of the bucket/object

**Example**

```bash
curl -fsSL \
    -XGET ${ENDPOINT}/buckets/example-bucket/objects/example/acl/anonymous \
    -H "Authorization: Bearer ${ACCESS_TOKEN}"
 
{"id":"anonymous","data":{"access":"r-"}}
```



## Update

Updates an ACL entry of the bucket/object.

**URI**

```
PUT /buckets/${BUCKET}/acl/${ACL_GROUP}
PUT /buckets/${BUCKET}/objects/${KEY}/acl/${ACL_GROUP}
```

**URI parameters**

Name      | Type   | Default    | Description
--------- | ------ | ---------- | ------------------
BUCKET    | string | _required_ | Name of the bucket
KEY       | string | _required_ | Key of the object
ACL_GROUP | string | _required_ | Name of the ACL group

**Payload**

`data` property of the ACL entry

**Response**

Modified ACL entry of the bucket/object

**Example**

```bash
curl -fsSL \
    -XPUT ${ENDPOINT}/buckets/example-bucket/objects/example/acl/anonymous \
    -H "Authorization: Bearer ${ACCESS_TOKEN}" \
    -H 'Content-Type: application/json' \
    -d '{"access":"r-","exp":32503680000}'
 
{"id":"anonymous","data":{"access":"r-"}}
```



## Delete

Removes an ACL entry of the bucket/object.

**URI**

```
DELETE /buckets/${BUCKET}/acl/${ACL_GROUP}
DELETE /buckets/${BUCKET}/objects/${KEY}/acl/${ACL_GROUP}
```

**URI parameters**

Name      | Type   | Default    | Description
--------- | ------ | ---------- | ------------------
BUCKET    | string | _required_ | Name of the bucket
KEY       | string | _required_ | Key of the object
ACL_GROUP | string | _required_ | Name of the ACL group

**Response**

Removed ACL entry of the bucket/object

**Example**

```bash
curl -fsSL \
    -XGET ${ENDPOINT}/buckets/example-bucket/objects/example/acl/anonymous \
    -H "Authorization: Bearer ${ACCESS_TOKEN}"
 
{"id":"anonymous","data":{"access":"r-"}}
```