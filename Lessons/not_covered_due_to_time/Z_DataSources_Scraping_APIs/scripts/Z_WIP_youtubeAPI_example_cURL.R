#' Title: Grab Youtube JSON using CURL request
#' Purpose: Demonstrate f12 in Chrome for API but use CURL instead of fromJSON
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 19, 2023
#' Instead of "copy link address" use "copy as cURL
#' Then use the https://curlconverter.com/r/

require(httr)

cookies = c(
  `VISITOR_INFO1_LIVE` = "ToxEp2PnuZg",
  `LOGIN_INFO` = "AFmmF2swRQIgZ81-_JHdjWGRvwN4aLc7k3dXK5Z-hO6nPy7sVFL26gcCIQDQXx_0fR2gV_d4x26s_cCPhv4ZxLOu9Voypjlu4P7tUw:QUQ3MjNmeU9VYW1MOUF6anZISE9JUGJzY1AxZE0tRXliUXRYcnUxQ1h4XzZyN1hFdUNIU1hnYlhRcDhsTjF3U0lFZEtyaHZrM19GSkszdklUX3Nfc3dUVW8zYWY1em4tc2F3bVRGSnJkbGVGX2FBckY0LUtDZlJYd1R6QnRralJ5RDhnUzRoQzBpRWtqN3JRQjJBQUZDUnJBMFZsZGY5WVR3",
  `VISITOR_PRIVACY_METADATA` = "CgJVUxICGgA=",
  `HSID` = "A1GMZlrr3rBJep7R4",
  `SSID` = "AazHqzXmMn_ppN_lm",
  `APISID` = "r1NWWhF6OhQPevdq/A6QNJjljy8O5CD37_",
  `SAPISID` = "6kXoIKh0OQkPujyu/ACQF8c08F58CDsuzE",
  `__Secure-1PAPISID` = "6kXoIKh0OQkPujyu/ACQF8c08F58CDsuzE",
  `__Secure-3PAPISID` = "6kXoIKh0OQkPujyu/ACQF8c08F58CDsuzE",
  `SID` = "cwjGaLWU_6Ut5-g1rP_NERnzqTrrh9y4XO1vNP8V24AqgnQDKhlzBnZZxM1fT5qEMHZi6w.",
  `__Secure-1PSID` = "cwjGaLWU_6Ut5-g1rP_NERnzqTrrh9y4XO1vNP8V24AqgnQDcLEtpT-J1ymNxxF1QGIAKQ.",
  `__Secure-3PSID` = "cwjGaLWU_6Ut5-g1rP_NERnzqTrrh9y4XO1vNP8V24AqgnQD_gRKr1UnqNXAEZjzXIpthQ.",
  `YSC` = "1xDFSePRSyg",
  `PREF` = "tz=America.New_York&f5=20000",
  `__Secure-1PSIDTS` = "sidts-CjIBNiGH7pIzwXHkftWR9x0XJJMvAbkhN085AKsjMo7cUFY5q73C1kVW7pwLJmtDYxFRIxAA",
  `__Secure-3PSIDTS` = "sidts-CjIBNiGH7pIzwXHkftWR9x0XJJMvAbkhN085AKsjMo7cUFY5q73C1kVW7pwLJmtDYxFRIxAA",
  `CONSISTENCY` = "AKreu9tv3eIakS2hMg5PLKeXhGZVUc_tkEXfdMFsSoZ5pYlOPV_gFa31SneGE7zBDzlyduLmzBQSr86xBdxojiIBduPA6OPYYnqJZXj6FMoAc8-vt39AEskuXQFi3tEszQNpkSOtmD_a_bmr-o_T7Lqx",
  `SIDCC` = "ACA-OxNMsOw-79N4TqqlPe8bc9idnKBFFGHw1vkgb3uWISWhfixGwaLjNPw5-b9eYkyeK1x3zV8",
  `__Secure-1PSIDCC` = "ACA-OxNAaetkIdDCfYy1wgKbFv7f_h2G-nyNnRLxj2ZYQPGg9pPAtEVf_ZQBCfSEe00RkfSVwno",
  `__Secure-3PSIDCC` = "ACA-OxNJwYmUGpfhLEzEZJGsKOZ2mEjvk5t53Qoc9alJA-2o42EL-_cQG_dQXM0d2PJpDHO33C8"
)

headers = c(
  `authority` = "www.youtube.com",
  `accept` = "*/*",
  `accept-language` = "en-US,en;q=0.9",
  `referer` = "https://www.youtube.com/watch?v=K5Rly83zfuI&ab_channel=TheDailyShow",
  `sec-ch-ua` = '"Google Chrome";v="119", "Chromium";v="119", "Not?A_Brand";v="24"',
  `sec-ch-ua-arch` = '"arm"',
  `sec-ch-ua-bitness` = '"64"',
  `sec-ch-ua-full-version` = '"119.0.6045.159"',
  `sec-ch-ua-full-version-list` = '"Google Chrome";v="119.0.6045.159", "Chromium";v="119.0.6045.159", "Not?A_Brand";v="24.0.0.0"',
  `sec-ch-ua-mobile` = "?0",
  `sec-ch-ua-model` = '""',
  `sec-ch-ua-platform` = '"macOS"',
  `sec-ch-ua-platform-version` = '"13.4.1"',
  `sec-ch-ua-wow64` = "?0",
  `sec-fetch-dest` = "empty",
  `sec-fetch-mode` = "cors",
  `sec-fetch-site` = "same-origin",
  `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36",
  `x-client-data` = "CI62yQEIo7bJAQipncoBCNiCywEIlKHLAQiGoM0BCO6xzQEI3L3NAQi7yM0BCOPazQEIzd/NAQi14M0BCMjgzQEI1ODNAQjg4c0BCNvjzQEI3unNAQixzcQiGPbJzQEYp+rNARjrjaUX",
  `x-goog-authuser` = "0",
  `x-goog-visitor-id` = "CgtUb3hFcDJQbnVaZyihtZSrBjIICgJVUxICGgA%3D",
  `x-youtube-ad-signals` = "dt=1701124770728&flash=0&frm&u_tz=-300&u_his=2&u_h=1080&u_w=1920&u_ah=1080&u_aw=1920&u_cd=24&bc=31&bih=735&biw=766&brdim=-318%2C-1022%2C-318%2C-1022%2C1920%2C-1080%2C1390%2C852%2C766%2C735&vis=1&wgl=true&ca_type=image&bid=ANyPxKo2qGewm3Td399PbBGnmwiEYjW1HT0HVDtWAK_2zo__3stQU5jGTRaeskeUa_vGNn2qvLmD3S2AjrkxzW6ooYGfbS2-3Q",
  `x-youtube-client-name` = "1",
  `x-youtube-client-version` = "2.20231121.08.00",
  `x-youtube-device` = "cbr=Chrome&cbrand=apple&cbrver=119.0.0.0&ceng=WebKit&cengver=537.36&cos=Macintosh&cosver=10_15_7&cplatform=DESKTOP",
  `x-youtube-identity-token` = "QUFFLUhqbWhodTBfeWVKcUZUaHZQbXlLQVFjNjZPLVpxZ3w=",
  `x-youtube-page-cl` = "584454497",
  `x-youtube-page-label` = "youtube.desktop.web_20231121_08_RC00",
  `x-youtube-time-zone` = "America/New_York",
  `x-youtube-utc-offset` = "-300"
)

params = list(
  `v` = "K5Rly83zfuI",
  `ei` = "oRplZffbKPaO_9EP7uu0-AM",
  `caps` = "asr",
  `opi` = "112496729",
  `xoaf` = "4",
  `hl` = "en",
  `ip` = "0.0.0.0",
  `ipbits` = "0",
  `expire` = "1701149969",
  `sparams` = "ip,ipbits,expire,v,ei,caps,opi,xoaf",
  `signature` = "6BDA74E8AFC34BB57F4CF68B2942E2406B8E39DB.671C2F6290AB89E8EC168F7F6628A25AFA91E4C8",
  `key` = "yt8",
  `lang` = "en-US",
  `fmt` = "json3",
  `xorb` = "2",
  `xobt` = "3",
  `xovt` = "3",
  `cbrand` = "apple",
  `cbr` = "Chrome",
  `cbrver` = "119.0.0.0",
  `c` = "WEB",
  `cver` = "2.20231121.08.00",
  `cplayer` = "UNIPLAYER",
  `cos` = "Macintosh",
  `cosver` = "10_15_7",
  `cplatform` = "DESKTOP"
)

res <- httr::GET(url = "https://www.youtube.com/api/timedtext", httr::add_headers(.headers=headers), query = params, httr::set_cookies(.cookies = cookies))



##### Underneath the curl convertor website code, now extract the response
apiResponse <- content(res)

# Examine a structure; just need some list extraction and clean up
str(apiResponse)
apiResponse

