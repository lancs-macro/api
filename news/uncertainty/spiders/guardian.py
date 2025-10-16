import scrapy
from scrapy.spiders.sitemap import SitemapSpider
from trafilatura import fetch_url, extract, bare_extraction
from ..items import ArticleItem

url = "https://www.theguardian.com/uk-news/2023/sep/26/sex-offender-avoids-jail-for-driving-car-into-downing-street-gates"
downloaded = fetch_url(url)
bare_extraction(
    downloaded,
    include_comments=False,
    include_tables=False,
    include_links=False,
)


class GuardianSpider(SitemapSpider):
    name = "guardian"
    allowed_domains = ["www.theguardian.com"]
    sitemap_urls = ["http://www.theguardian.com/sitemaps/news.xml"]
    sitemap_rules = [
        ("https://www.theguardian.com/uk", "parse"),
    ]

    def parse(self, response):
        result = bare_extraction(
            response.text,
            target_language="en",
        )
        yield ArticleItem(
            url=response.url,
            title=result["title"],
            body=result["text"],
            pub_date=result["date"],
        )
