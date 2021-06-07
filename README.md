# geoerl accesses the ipstack api using erlang.
Your access is defined in /include/geo.hrl and it should be a string

Installation:
1. Clone this repository
2. cd geoerl
3. rebar3 shell(rebar3 must be installed)

Examples:
To get all return values of the query
geoerl_app:all(IP) -> AllValues

To return specific values
geoerl_app:fetch(IP, ["location", "country_name", "country_code"]) -> [{"location", "Location"},{"country_name",CountryName"},{"country_code", CountryCode}].
