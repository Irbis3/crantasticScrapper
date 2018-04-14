# ------------------------------------------------- #
# Macropru governance indices from: https://www.imf.org/external/pubs/ft/wp/2013/wp13166.pdf
# Christopher Gandrud
# MIT License
# ------------------------------------------------- #

macro_gov <- data.frame(
    country = c(
        'Nigeria', 'China', 'Hong Kong', 'India',
        'Indonesia', 'Korea', 'Malaysia', 'Mongolia',
        'New Zealand', 'Singapore', 'Thailand', 'Vietnam',
        'Argentina', 'Brazil', 'Canada', 'Chile',
        'Colombia', 'Mexico', 'Peru', 'United States',
        'Uruguay', 'Austria', 'Bulgaria', 'Croatia',
        'Finland', 'Hungary', 'Netherlands', 'Norway',
        'Poland', 'Romania', 'Russia', 'Serbia',
        'Slovak Republic', 'Sweden', 'Turkey', 'Israel',
        'Kuwait', 'Lebanon', 'Saudi Arabia'
    ),
    mapp = c(
        3, 2, 2, 2,
        2, 2, 4, 2,
        4, 2, 4, 2,
        1, 2, 2, 2,
        2, 2, 1, 2,
        2, 2, 2, 4,
        2, 3, 2, 2,
        2, 3, 1, 4,
        4, 2, 2, 2,
        4, 3, 4
    ),
    mipp = c(
        2, 1, 2, 2,
        2, 1, 3, 2,
        3, 4, 2,
        2, 3, 2, 1,
        1, 1, 1, 1,
        2, 4, 2, 2,
        2, 1, 1, 3,
        1, 1, 2, 2,
        3, 2, 1, 1,
        2, 3, 2, 3
    ),
    mof = c(
        2, 4, 4, 2,
        2, 4, 2, 2,
        1, 4, 2,
        4, 1, 2, 4,
        2, 4, 2, 1,
        2, 2, 4, 4,
        1, 2, 3, 1,
        4, 2, 2, 1,
        2, 3, 1, 2,
        2, 1, 2, 1
    )
)
