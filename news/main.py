sources = [
    "https://www.theguardian.com/uk",
    "https://www.independent.co.uk/",
    "https://www.thetimes.co.uk/",
    "https://www.ft.com/world-uk",
    "https://www.dailymail.co.uk/home/index.html",
]

guardian = "http://www.theguardian.com/sitemaps/news.xml"
follow = "https://www.theguardian.com/uk"


independent = "https://www.independent.co.uk/sitemap.xml"
follow = "articles"

the_times = "https://www.thetimes.co.uk/sitemaps/archive/2023-09-4"


ft = "https://www.ft.com/sitemaps/index.xml"
follow = "https://www.ft.com/world-uk"


def generate_daily_mail_sitemap():
    years = [*range(1996, 2024)]
    return [
        f"https://www.dailymail.co.uk/sitemap-articles-year-{year}.xml"
        for year in years
    ]

generate_daily_mail_sitemap()
