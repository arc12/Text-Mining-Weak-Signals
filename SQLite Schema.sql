CREATE TABLE "abstract" (
"id" INTEGER PRIMARY KEY AUTOINCREMENT,
    "origin" TEXT NOT NULL,
    "year" TEXT NOT NULL,
    "pages" TEXT,
    "title" TEXT NOT NULL,
    "authors" TEXT NOT NULL,
    "abstract" TEXT NOT NULL,
    "keywords" TEXT,
    "url" TEXT NOT NULL,
    "dblp_url" TEXT,
    "pos_score" REAL,
    "neg_score" REAL,
    "subj_score" REAL,
    "econ_score" REAL,
    "polit_score" REAL,
    "legal_score" REAL,
    "doing_score" REAL,
    "knowing_score" REAL);
CREATE UNIQUE INDEX "ABSTRACT_URL_UNQ" on "abstract" (url ASC); 
CREATE INDEX "ABSTRACT_YEAR" on abstract (year ASC);

CREATE TABLE "blog_post" (
"id" INTEGER PRIMARY KEY AUTOINCREMENT,
    "content" TEXT NOT NULL,
    "title" TEXT NOT NULL,
    "authors" TEXT NOT NULL,
    "datestamp" TEXT NOT NULL,
    "origin" TEXT NOT NULL,
    "url" TEXT NOT NULL,
    "pos_score" REAL,
    "neg_score" REAL,
    "subj_score" REAL,
    "econ_score" REAL,
    "polit_score" REAL,
    "legal_score" REAL,
    "doing_score" REAL,
    "knowing_score" REAL
);
CREATE UNIQUE INDEX "BLOG_POST_URL_UNQ" on blog_post (url ASC); 
CREATE INDEX "BLOG_POST_DATESTAMP" on blog_post (datestamp ASC);
