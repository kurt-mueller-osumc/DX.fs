namespace DX.Api

open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open Newtonsoft.Json

module Base =
    module Response =
        module Error =
            /// An error response returned by the DNAnexus api
            type Json = 
                { error: {| ``type`` : string
                            message : string |} }
            
            /// Parse an error response into json
            let parseJson json =
                JsonConvert.DeserializeObject<Json>(json) 

        /// A response from DNAnexus API
        type Response<'a> =
            | InvalidAuthentication of Error.Json
            | ResourceNotFound of Error.Json
            | InvalidResponseType of string
            | Unknown of string
            | Valid of 'a

        /// Parse a response from the DNAnexus API
        let parse parseFn (response:HttpResponse) =
            match (response.StatusCode, response.Body) with
            | (401, Text errorJson) -> InvalidAuthentication (Error.parseJson errorJson)
            | (404, Text errorJson) -> ResourceNotFound (Error.parseJson errorJson)
            | (200, Text jsonText) -> Valid (parseFn jsonText)
            | (_ , Text responseText) -> Unknown responseText
            | (_, Binary bytes) -> InvalidResponseType ("Binary response: " + System.Text.Encoding.ASCII.GetString(bytes))

    /// The token used to access the DNAnexus API
    type ApiToken = ApiToken of string

    module Request =
        let headers (ApiToken apiToken) =
            [ ContentType HttpContentTypes.Json
              "Authorization", ("Bearer " + apiToken) ]

module DataObjects =
    type ProjectId = ProjectId of string
    type ObjectId = ObjectId of string

    type Request = {
        ApiToken: Base.ApiToken
        ProjectId: ProjectId
        StartingAt: ObjectId option
    }

    module Request =
        let serializeBody request =
            let (ProjectId projectId) = request.ProjectId
            let baseJson = {| scope = {| project = projectId
                                         recurse = true |}
                              describe = true |}

            match request.StartingAt with
                | Some (ObjectId objectId) ->
                    let json = {| baseJson with starting = {| project = projectId
                                                              id = objectId |} |}

                    JsonConvert.SerializeObject(json)

                | None ->
                    JsonConvert.SerializeObject(baseJson)
        
        let initiate request =
            Http.Request
                ( "https://api.dnanexus.com/system/findDataObjects",
                  httpMethod = "POST",
                  silentHttpErrors = true,
                  headers = Base.Request.headers request.ApiToken,
                  body = TextRequest (serializeBody request))

    module Response =
        type DataObjectJson = {
            project: string
            id: string
            describe: {| id: string
                         project: string
                         ``class``: string
                         sponsored: bool
                         name: string
                         types: string list
                         state: string
                         hidden: bool
                         links: string list
                         folder: string
                         tags: string list
                         created: uint64
                         modified: uint64
                         createdBy: {| user: string
                                       job: string
                                       executable: string |}
                         media: string
                         archivalState: string
                         size: uint64
                         cloudAccount: string |}
        }

        /// The json repsonse if the API call is successful
        type BodyJson = {
            /// The next file id to start the search at
            next: {| project: string
                     id: string |}
            results: DataObjectJson list
        }
 
        let deserializeBody jsonText =
            JsonConvert.DeserializeObject<BodyJson>(jsonText)
        
        let parse =
            Base.Response.parse deserializeBody

    let find = 
        Request.initiate >> Base.Response.parse Response.deserializeBody