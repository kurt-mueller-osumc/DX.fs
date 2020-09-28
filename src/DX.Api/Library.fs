namespace DX.Api

open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open Newtonsoft.Json

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
