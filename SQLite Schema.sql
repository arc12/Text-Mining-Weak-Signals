CREATE TABLE abstract (
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
    "subj_score" REAL
);
CREATE TABLE blog_post (
    "content" TEXT NOT NULL,
    "title" TEXT NOT NULL,
    "authors" TEXT NOT NULL,
    "datestamp" TEXT NOT NULL,
    "origin" TEXT NOT NULL,
    "url" TEXT NOT NULL,
    "pos_score" REAL,
    "neg_score" REAL,
    "subj_score" REAL
);
CREATE UNIQUE INDEX "ABSTRACT_URL_UNQ" on "abstract" (url ASC);
CREATE UNIQUE INDEX "BLOG_POST_URL_UNQ" on blog_post (url ASC);
CREATE TABLE sqlite_stat1(tbl,idx,stat);
CREATE INDEX "BLOG_POST_DATESTAMP" on blog_post (datestamp ASC);
CREATE INDEX "ABSTRACT_YEAR" on abstract (year ASC);
