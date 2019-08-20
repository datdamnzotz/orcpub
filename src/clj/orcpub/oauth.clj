(ns orcpub.oauth
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [orcpub.route-map :as route-map]))

#_(def google-client-id "753086155056-fp9804v86fbf1vk3t4i0ql8polalcsn7.apps.googleusercontent.com")
#_(def google-client-secret "xiQ691JP9S89OO5uKVmIt1as")
#_(def google-oauth-url (str "https://accounts.google.com/o/oauth2/v2/auth?client_id=" google-client-id "&response_type=code&scope=email&redirect_uri="))
#_(def google-token-url "https://www.googleapis.com/oauth2/v4/token")

#_(def fb-client-id "1673290702980265")
#_(def fb-client-secret "2377805fe291bbd9a40ee3d42ee46b90")
#_(def fb-oauth-url (str "https://www.facebook.com/dialog/oauth?client_id=" fb-client-id "&scope=email&redirect_uri="))
#_(def fb-token-url "https://graph.facebook.com/v2.3/oauth/access_token?")

#_(defn app-id [url]
  (if (clojure.string/starts-with? url "http://localhost")
    "1994900940729588"
    fb-client-id))

(defn base-url [protocol host & [port]]
  (str protocol "//" host (when port (str ":" port))))

(defn get-base-url [req]
  (base-url (str (name (:scheme req)) ":") ((:headers req) "host")))

#_(defn get-fb-redirect-uri [req]
  (str (get-base-url req) (route-map/path-for route-map/fb-login-route)))

#_(defn get-google-redirect-uri [req]
  (str (get-base-url req) (route-map/path-for route-map/google-login-route)))

#_(defn get-fb-user [access-token]
  (let [resp (client/get
              "https://graph.facebook.com/me"
              {:query-params {:access_token access-token
                              :fields "email"
                              :debug "all"}})
        body (json/parse-string (:body resp) true)]
    body))

#_(defn get-google-user [access-token]
  (let [resp (client/get
             "https://www.googleapis.com/oauth2/v2/userinfo"
             {:query-params {:access_token access-token :fields "email"}})
        body (json/parse-string (:body resp) true)]
    body))

#_(defn get-fb-access-token [code redirect-uri]
  (let [resp (client/get fb-token-url
                         {:throw-exceptions false
                          :query-params
                          {:client_id fb-client-id
                           :redirect_uri redirect-uri
                           :client_secret fb-client-secret
                           :code code}})
        body (json/parse-string (:body resp) true)
        access-token (:access_token body)]
    access-token))

#_(defn get-google-access-token [code redirect-uri]
  (let [resp (client/post google-token-url {:query-params {:client_id google-client-id :redirect_uri redirect-uri :client_secret google-client-secret :code code :grant_type "authorization_code"}})
             body (json/parse-string (:body resp) true)
             access-token (:access_token body)]
         access-token))

#_(defn get-fb-email [{:keys [query-params] :as request}]
  (let [redirect-uri (get-fb-redirect-uri request)
        access-token (get-fb-access-token (:code query-params) redirect-uri)
        fb-user (get-fb-user access-token)
        email (:email fb-user)]
    email))

#_(defn get-google-email [{:keys [query-params] :as request}]
  (let [redirect-uri (get-google-redirect-uri request)
        access-token (get-google-access-token (:code query-params) redirect-uri)
        google-user (get-google-user access-token)
        email (:email google-user)]
    email))
