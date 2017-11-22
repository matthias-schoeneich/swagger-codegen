//
// FakeclassnametagsAPI.swift
//
// Generated by swagger-codegen
// https://github.com/swagger-api/swagger-codegen
//

import Alamofire
import PromiseKit



open class FakeclassnametagsAPI {
    /**
     To test class name in snake case
     
     - parameter body: (body) client model 
     - parameter completion: completion handler to receive the data and the error objects
     */
    open class func testClassname(body: Client, completion: @escaping ((_ data: Client?,_ error: Error?) -> Void)) {
        testClassnameWithRequestBuilder(body: body).execute { (response, error) -> Void in
            completion(response?.body, error);
        }
    }

    /**
     To test class name in snake case
     
     - parameter body: (body) client model 
     - returns: Promise<Client>
     */
    open class func testClassname( body: Client) -> Promise<Client> {
        let deferred = Promise<Client>.pending()
        testClassname(body: body) { data, error in
            if let error = error {
                deferred.reject(error)
            } else {
                deferred.fulfill(data!)
            }
        }
        return deferred.promise
    }

    /**
     To test class name in snake case
     - PATCH /fake_classname_test
     - examples: [{contentType=application/json, example={
  "client" : "aeiou"
}}]
     
     - parameter body: (body) client model 

     - returns: RequestBuilder<Client> 
     */
    open class func testClassnameWithRequestBuilder(body: Client) -> RequestBuilder<Client> {
        let path = "/fake_classname_test"
        let URLString = PetstoreClientAPI.basePath + path
        let parameters = JSONEncodingHelper.encodingParameters(forEncodableObject: body)
 
        let convertedParameters = APIHelper.convertBoolToString(parameters)
 
        let requestBuilder: RequestBuilder<Client>.Type = PetstoreClientAPI.requestBuilderFactory.getBuilder()

        return requestBuilder.init(method: "PATCH", URLString: URLString, parameters: convertedParameters, isBody: true)
    }

}
