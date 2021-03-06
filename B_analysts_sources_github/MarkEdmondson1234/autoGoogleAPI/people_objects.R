#' Google People API Objects 
#' Provides access to information about profiles and contacts.
#' 
#' Auto-generated code by googleAuthR::gar_create_api_objects
#'  at 2017-03-05 20:04:23
#' filename: /Users/mark/dev/R/autoGoogleAPI/googlepeoplev1.auto/R/people_objects.R
#' api_json: api_json
#' 
#' Objects for use by the functions created by googleAuthR::gar_create_api_skeleton

#' Name Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's name. If the name is a mononym, the family name is empty.
#' 
#' @param honorificPrefix The honorific prefixes, such as `Mrs
#' @param phoneticHonorificSuffix The honorific suffixes spelled as they sound
#' @param givenName The given name
#' @param middleName The middle name(s)
#' @param phoneticHonorificPrefix The honorific prefixes spelled as they sound
#' @param phoneticGivenName The given name spelled as it sounds
#' @param phoneticFamilyName The family name spelled as it sounds
#' @param familyName The family name
#' @param metadata Metadata about the name
#' @param phoneticMiddleName The middle name(s) spelled as they sound
#' @param phoneticFullName The full name spelled as it sounds
#' @param displayNameLastFirst The read-only display name with the last name first formatted according to
#' @param displayName The read-only display name formatted according to the locale specified by
#' @param honorificSuffix The honorific suffixes, such as `Jr
#' 
#' @return Name object
#' 
#' @family Name functions
#' @export
Name <- function(honorificPrefix = NULL, phoneticHonorificSuffix = NULL, givenName = NULL, 
    middleName = NULL, phoneticHonorificPrefix = NULL, phoneticGivenName = NULL, 
    phoneticFamilyName = NULL, familyName = NULL, metadata = NULL, phoneticMiddleName = NULL, 
    phoneticFullName = NULL, displayNameLastFirst = NULL, displayName = NULL, honorificSuffix = NULL) {
    structure(list(honorificPrefix = honorificPrefix, phoneticHonorificSuffix = phoneticHonorificSuffix, 
        givenName = givenName, middleName = middleName, phoneticHonorificPrefix = phoneticHonorificPrefix, 
        phoneticGivenName = phoneticGivenName, phoneticFamilyName = phoneticFamilyName, 
        familyName = familyName, metadata = metadata, phoneticMiddleName = phoneticMiddleName, 
        phoneticFullName = phoneticFullName, displayNameLastFirst = displayNameLastFirst, 
        displayName = displayName, honorificSuffix = honorificSuffix), class = "gar_Name")
}

#' BraggingRights Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's bragging rights.
#' 
#' @param metadata Metadata about the bragging rights
#' @param value The bragging rights; for example, `climbed mount everest`
#' 
#' @return BraggingRights object
#' 
#' @family BraggingRights functions
#' @export
BraggingRights <- function(metadata = NULL, value = NULL) {
    structure(list(metadata = metadata, value = value), class = "gar_BraggingRights")
}

#' Locale Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's locale preference.
#' 
#' @param metadata Metadata about the locale
#' @param value The well-formed [IETF BCP 47](https://tools
#' 
#' @return Locale object
#' 
#' @family Locale functions
#' @export
Locale <- function(metadata = NULL, value = NULL) {
    structure(list(metadata = metadata, value = value), class = "gar_Locale")
}

#' Organization Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's past or current organization. Overlapping date ranges arepermitted.
#' 
#' @param endDate The end date when the person left the organization
#' @param symbol The symbol associated with the organization; for example, a stock ticker
#' @param name The name of the organization
#' @param metadata Metadata about the organization
#' @param location The location of the organization office the person works at
#' @param title The person's job title at the organization
#' @param current True if the organization is the person's current organization;
#' @param formattedType The read-only type of the organization translated and formatted in the
#' @param startDate The start date when the person joined the organization
#' @param domain The domain name associated with the organization; for example, `google
#' @param department The person's department at the organization
#' @param phoneticName The phonetic name of the organization
#' @param type The type of the organization
#' @param jobDescription The person's job description at the organization
#' 
#' @return Organization object
#' 
#' @family Organization functions
#' @export
Organization <- function(endDate = NULL, symbol = NULL, name = NULL, metadata = NULL, 
    location = NULL, title = NULL, current = NULL, formattedType = NULL, startDate = NULL, 
    domain = NULL, department = NULL, phoneticName = NULL, type = NULL, jobDescription = NULL) {
    structure(list(endDate = endDate, symbol = symbol, name = name, metadata = metadata, 
        location = location, title = title, current = current, formattedType = formattedType, 
        startDate = startDate, domain = domain, department = department, phoneticName = phoneticName, 
        type = type, jobDescription = jobDescription), class = "gar_Organization")
}

#' Biography Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's short biography.
#' 
#' @param contentType The content type of the biography
#' @param metadata Metadata about the biography
#' @param value The short biography
#' 
#' @return Biography object
#' 
#' @family Biography functions
#' @export
Biography <- function(contentType = NULL, metadata = NULL, value = NULL) {
    structure(list(contentType = contentType, metadata = metadata, value = value), 
        class = "gar_Biography")
}

#' AgeRangeType Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's age range.
#' 
#' @param metadata Metadata about the age range
#' @param ageRange The age range
#' 
#' @return AgeRangeType object
#' 
#' @family AgeRangeType functions
#' @export
AgeRangeType <- function(metadata = NULL, ageRange = NULL) {
    structure(list(metadata = metadata, ageRange = ageRange), class = "gar_AgeRangeType")
}

#' FieldMetadata Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Metadata about a field.
#' 
#' @param source The source of the field
#' @param verified True if the field is verified; false if the field is unverified
#' @param primary True if the field is the primary field; false if the field is a secondary
#' 
#' @return FieldMetadata object
#' 
#' @family FieldMetadata functions
#' @export
FieldMetadata <- function(source = NULL, verified = NULL, primary = NULL) {
    structure(list(source = source, verified = verified, primary = primary), class = "gar_FieldMetadata")
}

#' Source Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The source of a field.
#' 
#' @param profileMetadata Metadata about a source of type PROFILE
#' @param type The source type
#' @param etag The [HTTP entity tag](https://en
#' @param id The unique identifier within the source type generated by the server
#' 
#' @return Source object
#' 
#' @family Source functions
#' @export
Source <- function(profileMetadata = NULL, type = NULL, etag = NULL, id = NULL) {
    structure(list(profileMetadata = profileMetadata, type = type, etag = etag, id = id), 
        class = "gar_Source")
}

#' PersonResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The response for a single person
#' 
#' @param person The person
#' @param httpStatusCode [HTTP 1
#' @param requestedResourceName The original requested resource name
#' 
#' @return PersonResponse object
#' 
#' @family PersonResponse functions
#' @export
PersonResponse <- function(person = NULL, httpStatusCode = NULL, requestedResourceName = NULL) {
    structure(list(person = person, httpStatusCode = httpStatusCode, requestedResourceName = requestedResourceName), 
        class = "gar_PersonResponse")
}

#' RelationshipInterest Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's read-only relationship interest .
#' 
#' @param metadata Metadata about the relationship interest
#' @param value The kind of relationship the person is looking for
#' @param formattedValue The value of the relationship interest translated and formatted in the
#' 
#' @return RelationshipInterest object
#' 
#' @family RelationshipInterest functions
#' @export
RelationshipInterest <- function(metadata = NULL, value = NULL, formattedValue = NULL) {
    structure(list(metadata = metadata, value = value, formattedValue = formattedValue), 
        class = "gar_RelationshipInterest")
}

#' Relation Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's relation to another person.
#' 
#' @param metadata Metadata about the relation
#' @param type The person's relation to the other person
#' @param person The name of the other person this relation refers to
#' @param formattedType The type of the relation translated and formatted in the viewer's account
#' 
#' @return Relation object
#' 
#' @family Relation functions
#' @export
Relation <- function(metadata = NULL, type = NULL, person = NULL, formattedType = NULL) {
    structure(list(metadata = metadata, type = type, person = person, formattedType = formattedType), 
        class = "gar_Relation")
}

#' Occupation Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's occupation.
#' 
#' @param metadata Metadata about the occupation
#' @param value The occupation; for example, `carpenter`
#' 
#' @return Occupation object
#' 
#' @family Occupation functions
#' @export
Occupation <- function(metadata = NULL, value = NULL) {
    structure(list(metadata = metadata, value = value), class = "gar_Occupation")
}

#' Person Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Information about a person merged from various data sources such as theauthenticated user's contacts and profile data. Fields other than IDs,metadata, and group memberships are user-edited.Most fields can have multiple items. The items in a field have no guaranteedorder, but each non-empty field is guaranteed to have exactly one field with`metadata.primary` set to true.NEXT_ID: 31
#' 
#' @param nicknames The person's nicknames
#' @param names The person's names
#' @param relations The person's relations
#' @param occupations The person's occupations
#' @param emailAddresses The person's email addresses
#' @param organizations The person's past or current organizations
#' @param etag The [HTTP entity tag](https://en
#' @param braggingRights The person's bragging rights
#' @param metadata Metadata about the person
#' @param residences The person's residences
#' @param genders The person's genders
#' @param resourceName The resource name for the person, assigned by the server
#' @param interests The person's interests
#' @param biographies The person's biographies
#' @param skills The person's skills
#' @param relationshipStatuses The person's relationship statuses
#' @param photos The person's photos
#' @param ageRange DEPRECATED(Please read person
#' @param taglines The person's taglines
#' @param ageRanges The person's age ranges
#' @param addresses The person's street addresses
#' @param events The person's events
#' @param memberships The person's group memberships
#' @param phoneNumbers The person's phone numbers
#' @param coverPhotos The person's cover photos
#' @param imClients The person's instant messaging clients
#' @param birthdays The person's birthdays
#' @param locales The person's locale preferences
#' @param relationshipInterests The kind of relationship the person is looking for
#' @param urls The person's associated URLs
#' 
#' @return Person object
#' 
#' @family Person functions
#' @export
Person <- function(nicknames = NULL, names = NULL, relations = NULL, occupations = NULL, 
    emailAddresses = NULL, organizations = NULL, etag = NULL, braggingRights = NULL, 
    metadata = NULL, residences = NULL, genders = NULL, resourceName = NULL, interests = NULL, 
    biographies = NULL, skills = NULL, relationshipStatuses = NULL, photos = NULL, 
    ageRange = NULL, taglines = NULL, ageRanges = NULL, addresses = NULL, events = NULL, 
    memberships = NULL, phoneNumbers = NULL, coverPhotos = NULL, imClients = NULL, 
    birthdays = NULL, locales = NULL, relationshipInterests = NULL, urls = NULL) {
    structure(list(nicknames = nicknames, names = names, relations = relations, occupations = occupations, 
        emailAddresses = emailAddresses, organizations = organizations, etag = etag, 
        braggingRights = braggingRights, metadata = metadata, residences = residences, 
        genders = genders, resourceName = resourceName, interests = interests, biographies = biographies, 
        skills = skills, relationshipStatuses = relationshipStatuses, photos = photos, 
        ageRange = ageRange, taglines = taglines, ageRanges = ageRanges, addresses = addresses, 
        events = events, memberships = memberships, phoneNumbers = phoneNumbers, 
        coverPhotos = coverPhotos, imClients = imClients, birthdays = birthdays, 
        locales = locales, relationshipInterests = relationshipInterests, urls = urls), 
        class = "gar_Person")
}

#' GetPeopleResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' No description
#' 
#' @param responses The response for each requested resource name
#' 
#' @return GetPeopleResponse object
#' 
#' @family GetPeopleResponse functions
#' @export
GetPeopleResponse <- function(responses = NULL) {
    structure(list(responses = responses), class = "gar_GetPeopleResponse")
}

#' Photo Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's read-only photo. A picture shown next to the person's name tohelp others recognize the person.
#' 
#' @param metadata Metadata about the photo
#' @param url The URL of the photo
#' 
#' @return Photo object
#' 
#' @family Photo functions
#' @export
Photo <- function(metadata = NULL, url = NULL) {
    structure(list(metadata = metadata, url = url), class = "gar_Photo")
}

#' PhoneNumber Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's phone number.
#' 
#' @param metadata Metadata about the phone number
#' @param type The type of the phone number
#' @param value The phone number
#' @param formattedType The read-only type of the phone number translated and formatted in the
#' @param canonicalForm The read-only canonicalized [ITU-T E
#' 
#' @return PhoneNumber object
#' 
#' @family PhoneNumber functions
#' @export
PhoneNumber <- function(metadata = NULL, type = NULL, value = NULL, formattedType = NULL, 
    canonicalForm = NULL) {
    structure(list(metadata = metadata, type = type, value = value, formattedType = formattedType, 
        canonicalForm = canonicalForm), class = "gar_PhoneNumber")
}

#' ListConnectionsResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' No description
#' 
#' @param nextPageToken The token that can be used to retrieve the next page of results
#' @param connections The list of people that the requestor is connected to
#' @param nextSyncToken The token that can be used to retrieve changes since the last request
#' 
#' @return ListConnectionsResponse object
#' 
#' @family ListConnectionsResponse functions
#' @export
ListConnectionsResponse <- function(nextPageToken = NULL, connections = NULL, nextSyncToken = NULL) {
    structure(list(nextPageToken = nextPageToken, connections = connections, nextSyncToken = nextSyncToken), 
        class = "gar_ListConnectionsResponse")
}

#' Birthday Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's birthday. At least one of the `date` and `text` fields arespecified. The `date` and `text` fields typically represent the samedate, but are not guaranteed to.
#' 
#' @param metadata Metadata about the birthday
#' @param text A free-form string representing the user's birthday
#' @param date The date of the birthday
#' 
#' @return Birthday object
#' 
#' @family Birthday functions
#' @export
Birthday <- function(metadata = NULL, text = NULL, date = NULL) {
    structure(list(metadata = metadata, text = text, date = date), class = "gar_Birthday")
}

#' Residence Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's past or current residence.
#' 
#' @param metadata Metadata about the residence
#' @param current True if the residence is the person's current residence;
#' @param value The address of the residence
#' 
#' @return Residence object
#' 
#' @family Residence functions
#' @export
Residence <- function(metadata = NULL, current = NULL, value = NULL) {
    structure(list(metadata = metadata, current = current, value = value), class = "gar_Residence")
}

#' Address Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's physical address. May be a P.O. box or street address. All fieldsare optional.
#' 
#' @param streetAddress The street address
#' @param metadata Metadata about the address
#' @param countryCode The [ISO 3166-1 alpha-2](http://www
#' @param formattedType The read-only type of the address translated and formatted in the viewer's
#' @param city The city of the address
#' @param formattedValue The unstructured value of the address
#' @param country The country of the address
#' @param type The type of the address
#' @param extendedAddress The extended address of the address; for example, the apartment number
#' @param poBox The P
#' @param postalCode The postal code of the address
#' @param region The region of the address; for example, the state or province
#' 
#' @return Address object
#' 
#' @family Address functions
#' @export
Address <- function(streetAddress = NULL, metadata = NULL, countryCode = NULL, formattedType = NULL, 
    city = NULL, formattedValue = NULL, country = NULL, type = NULL, extendedAddress = NULL, 
    poBox = NULL, postalCode = NULL, region = NULL) {
    structure(list(streetAddress = streetAddress, metadata = metadata, countryCode = countryCode, 
        formattedType = formattedType, city = city, formattedValue = formattedValue, 
        country = country, type = type, extendedAddress = extendedAddress, poBox = poBox, 
        postalCode = postalCode, region = region), class = "gar_Address")
}

#' ContactGroupMembership Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A Google contact group membership.
#' 
#' @param contactGroupId The contact group ID for the contact group membership
#' 
#' @return ContactGroupMembership object
#' 
#' @family ContactGroupMembership functions
#' @export
ContactGroupMembership <- function(contactGroupId = NULL) {
    structure(list(contactGroupId = contactGroupId), class = "gar_ContactGroupMembership")
}

#' Event Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' An event related to the person.
#' 
#' @param metadata Metadata about the event
#' @param type The type of the event
#' @param date The date of the event
#' @param formattedType The read-only type of the event translated and formatted in the
#' 
#' @return Event object
#' 
#' @family Event functions
#' @export
Event <- function(metadata = NULL, type = NULL, date = NULL, formattedType = NULL) {
    structure(list(metadata = metadata, type = type, date = date, formattedType = formattedType), 
        class = "gar_Event")
}

#' PersonMetadata Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The read-only metadata about a person.
#' 
#' @param sources The sources of data for the person
#' @param previousResourceNames Any former resource names this person has had
#' @param deleted True if the person resource has been deleted
#' @param objectType DEPRECATED(Please read person
#' @param linkedPeopleResourceNames Resource names of people linked to this resource
#' 
#' @return PersonMetadata object
#' 
#' @family PersonMetadata functions
#' @export
PersonMetadata <- function(sources = NULL, previousResourceNames = NULL, deleted = NULL, 
    objectType = NULL, linkedPeopleResourceNames = NULL) {
    structure(list(sources = sources, previousResourceNames = previousResourceNames, 
        deleted = deleted, objectType = objectType, linkedPeopleResourceNames = linkedPeopleResourceNames), 
        class = "gar_PersonMetadata")
}

#' ProfileMetadata Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The read-only metadata about a profile.
#' 
#' @param objectType The profile object type
#' 
#' @return ProfileMetadata object
#' 
#' @family ProfileMetadata functions
#' @export
ProfileMetadata <- function(objectType = NULL) {
    structure(list(objectType = objectType), class = "gar_ProfileMetadata")
}

#' Url Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's associated URLs.
#' 
#' @param formattedType The read-only type of the URL translated and formatted in the viewer's
#' @param metadata Metadata about the URL
#' @param type The type of the URL
#' @param value The URL
#' 
#' @return Url object
#' 
#' @family Url functions
#' @export
Url <- function(formattedType = NULL, metadata = NULL, type = NULL, value = NULL) {
    structure(list(formattedType = formattedType, metadata = metadata, type = type, 
        value = value), class = "gar_Url")
}

#' Gender Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's gender.
#' 
#' @param formattedValue The read-only value of the gender translated and formatted in the viewer's
#' @param metadata Metadata about the gender
#' @param value The gender for the person
#' 
#' @return Gender object
#' 
#' @family Gender functions
#' @export
Gender <- function(formattedValue = NULL, metadata = NULL, value = NULL) {
    structure(list(formattedValue = formattedValue, metadata = metadata, value = value), 
        class = "gar_Gender")
}

#' CoverPhoto Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's read-only cover photo. A large image shown on the person'sprofile page that represents who they are or what they care about.
#' 
#' @param metadata Metadata about the cover photo
#' @param default True if the cover photo is the default cover photo;
#' @param url The URL of the cover photo
#' 
#' @return CoverPhoto object
#' 
#' @family CoverPhoto functions
#' @export
CoverPhoto <- function(metadata = NULL, default = NULL, url = NULL) {
    structure(list(metadata = metadata, default = default, url = url), class = "gar_CoverPhoto")
}

#' Interest Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' One of the person's interests.
#' 
#' @param metadata Metadata about the interest
#' @param value The interest; for example, `stargazing`
#' 
#' @return Interest object
#' 
#' @family Interest functions
#' @export
Interest <- function(metadata = NULL, value = NULL) {
    structure(list(metadata = metadata, value = value), class = "gar_Interest")
}

#' ImClient Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's instant messaging client.
#' 
#' @param formattedProtocol The read-only protocol of the IM client formatted in the viewer's account
#' @param formattedType The read-only type of the IM client translated and formatted in the
#' @param metadata Metadata about the IM client
#' @param type The type of the IM client
#' @param protocol The protocol of the IM client
#' @param username The user name used in the IM client
#' 
#' @return ImClient object
#' 
#' @family ImClient functions
#' @export
ImClient <- function(formattedProtocol = NULL, formattedType = NULL, metadata = NULL, 
    type = NULL, protocol = NULL, username = NULL) {
    structure(list(formattedProtocol = formattedProtocol, formattedType = formattedType, 
        metadata = metadata, type = type, protocol = protocol, username = username), 
        class = "gar_ImClient")
}

#' Nickname Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's nickname.
#' 
#' @param metadata Metadata about the nickname
#' @param type The type of the nickname
#' @param value The nickname
#' 
#' @return Nickname object
#' 
#' @family Nickname functions
#' @export
Nickname <- function(metadata = NULL, type = NULL, value = NULL) {
    structure(list(metadata = metadata, type = type, value = value), class = "gar_Nickname")
}

#' EmailAddress Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's email address.
#' 
#' @param displayName The display name of the email
#' @param metadata Metadata about the email address
#' @param type The type of the email address
#' @param value The email address
#' @param formattedType The read-only type of the email address translated and formatted in the
#' 
#' @return EmailAddress object
#' 
#' @family EmailAddress functions
#' @export
EmailAddress <- function(displayName = NULL, metadata = NULL, type = NULL, value = NULL, 
    formattedType = NULL) {
    structure(list(displayName = displayName, metadata = metadata, type = type, value = value, 
        formattedType = formattedType), class = "gar_EmailAddress")
}

#' Skill Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A skill that the person has.
#' 
#' @param metadata Metadata about the skill
#' @param value The skill; for example, `underwater basket weaving`
#' 
#' @return Skill object
#' 
#' @family Skill functions
#' @export
Skill <- function(metadata = NULL, value = NULL) {
    structure(list(metadata = metadata, value = value), class = "gar_Skill")
}

#' DomainMembership Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A Google Apps Domain membership.
#' 
#' @param inViewerDomain True if the person is in the viewer's Google Apps domain
#' 
#' @return DomainMembership object
#' 
#' @family DomainMembership functions
#' @export
DomainMembership <- function(inViewerDomain = NULL) {
    structure(list(inViewerDomain = inViewerDomain), class = "gar_DomainMembership")
}

#' Membership Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's read-only membership in a group.
#' 
#' @param contactGroupMembership The contact group membership
#' @param domainMembership The domain membership
#' @param metadata Metadata about the membership
#' 
#' @return Membership object
#' 
#' @family Membership functions
#' @export
Membership <- function(contactGroupMembership = NULL, domainMembership = NULL, metadata = NULL) {
    structure(list(contactGroupMembership = contactGroupMembership, domainMembership = domainMembership, 
        metadata = metadata), class = "gar_Membership")
}

#' RelationshipStatus Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A person's read-only relationship status.
#' 
#' @param metadata Metadata about the relationship status
#' @param value The relationship status
#' @param formattedValue The read-only value of the relationship status translated and formatted in
#' 
#' @return RelationshipStatus object
#' 
#' @family RelationshipStatus functions
#' @export
RelationshipStatus <- function(metadata = NULL, value = NULL, formattedValue = NULL) {
    structure(list(metadata = metadata, value = value, formattedValue = formattedValue), 
        class = "gar_RelationshipStatus")
}

#' Tagline Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A read-only brief one-line description of the person.
#' 
#' @param metadata Metadata about the tagline
#' @param value The tagline
#' 
#' @return Tagline object
#' 
#' @family Tagline functions
#' @export
Tagline <- function(metadata = NULL, value = NULL) {
    structure(list(metadata = metadata, value = value), class = "gar_Tagline")
}


#' Date Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Represents a whole calendar date, for example a date of birth. The timeof day and time zone are either specified elsewhere or are notsignificant. The date is relative to the[Proleptic Gregorian Calendar](https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar).The day may be 0 to represent a year and month where the day is notsignificant. The year may be 0 to represent a month and day independentof year; for example, anniversary date.
#' 
#' @param year Year of date
#' @param day Day of month
#' @param month Month of year
#' 
#' @return Date object
#' 
#' @family Date functions
#' @export


Date <- function(year = NULL, day = NULL, month = NULL) {
    
    
    
    structure(list(year = year, day = day, month = month), class = "gar_Date")
}

