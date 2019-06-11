DROP FUNCTION IF EXISTS is_frozen;
DROP FUNCTION IF EXISTS is_unique;
DROP FUNCTION IF EXISTS member_exists;
DROP FUNCTION IF EXISTS project_exists;
DROP FUNCTION IF EXISTS authority_exists;
DROP FUNCTION IF EXISTS correct_password;
DROP FUNCTION IF EXISTS action_exists;
DROP FUNCTION IF EXISTS is_leader;
DROP FUNCTION IF EXISTS trolls;
DROP FUNCTION IF EXISTS vote_exists;
DROP TABLE IF EXISTS Vote;
DROP TABLE IF EXISTS Action;
DROP TABLE IF EXISTS Member;
DROP TABLE IF EXISTS Project;
DROP TABLE IF EXISTS Authority;
DROP ROLE IF EXISTS app;
-- for debugging

CREATE TABLE Authority (
    id BIGINT PRIMARY KEY
);

CREATE TABLE Project (
    id          BIGINT PRIMARY KEY,
    authorityid BIGINT NOT NULL,  

    CONSTRAINT project_authorityid_fkey 
        FOREIGN KEY (authorityid) 
        REFERENCES Authority (id)
);

CREATE TABLE Member (
    id            BIGINT    PRIMARY KEY,
    passwd        VARCHAR   NOT NULL,
    last_activity TIMESTAMP NOT NULL,
    is_leader     BOOLEAN   DEFAULT false NOT NULL
);

CREATE TABLE Action (
    id         BIGINT  PRIMARY KEY,
    is_support BOOLEAN NOT NULL,
    projectid  BIGINT  NOT NULL,
    memberid   BIGINT  NOT NULL,
    upvotes    BIGINT  DEFAULT 0 NOT NULL,
    downvotes BIGINT  DEFAULT 0 NOT NULL,

    CONSTRAINT action_projectid_fkey
        FOREIGN KEY (projectid)
        REFERENCES Project (id),
    
    CONSTRAINT action_memberid_fkey
        FOREIGN KEY (memberid)
        REFERENCES Member (id)
);

CREATE TABLE Vote (
    memberid  BIGINT  NOT NULL,
    actionid  BIGINT  NOT NULL,
    is_upvote BOOLEAN NOT NULL,

    PRIMARY KEY (memberid, actionid),

    CONSTRAINT vote_memberid_fkey
        FOREIGN KEY (memberid)
        REFERENCES Member (id),

    CONSTRAINT vote_actionid_fkey
        FOREIGN KEY (actionid)
        REFERENCES Action (id)
);

CREATE FUNCTION is_unique(BIGINT)
    RETURNS BOOLEAN AS $X$
    SELECT $1 NOT IN (SELECT id FROM Member)    AND
           $1 NOT IN (SELECT id FROM Authority) AND
           $1 NOT IN (SELECT id FROM Project)   AND
           $1 NOT IN (SELECT id FROM Action)
$X$ LANGUAGE SQL STABLE;

CREATE FUNCTION is_frozen(BIGINT, BIGINT)
    RETURNS BOOLEAN AS $X$
    SELECT (to_timestamp($2) - last_activity) > '365 days'
    FROM Member
    WHERE id = $1
$X$ LANGUAGE SQL STABLE;

CREATE FUNCTION correct_password(BIGINT, VARCHAR)
    RETURNS BOOLEAN AS $X$
    WITH pswhash AS (SELECT passwd FROM Member WHERE id=$1)
    SELECT crypt($2, passwd) = passwd FROM pswhash
$X$ LANGUAGE SQL STABLE;

CREATE FUNCTION member_exists(BIGINT)
    RETURNS BOOLEAN AS $X$
    SELECT $1 IN (SELECT id FROM Member)
$X$ LANGUAGE SQL STABLE;

CREATE FUNCTION vote_exists(m BIGINT, a BIGINT)
    RETURNS BOOLEAN AS $X$
    SELECT (m,a) IN (SELECT memberid, actionid FROM Vote)
$X$ LANGUAGE SQL STABLE;

CREATE FUNCTION project_exists(BIGINT)
    RETURNS BOOLEAN AS $X$
    SELECT $1 IN (SELECT id FROM Project)
$X$ LANGUAGE SQL STABLE;

CREATE FUNCTION authority_exists(BIGINT)
    RETURNS BOOLEAN AS $X$
    SELECT $1 IN (SELECT id FROM Authority)
$X$ LANGUAGE SQL STABLE;

CREATE FUNCTION action_exists(BIGINT)
    RETURNS BOOLEAN AS $X$
    SELECT $1 IN (SELECT id FROM Action)
$X$ LANGUAGE SQL STABLE;

CREATE FUNCTION is_leader(BIGINT)
    RETURNS BOOLEAN AS $X$
    SELECT is_leader FROM Member WHERE id = $1
$X$ LANGUAGE SQL STABLE;

CREATE USER app WITH ENCRYPTED PASSWORD 'qwerty';
GRANT SELECT, INSERT, UPDATE ON ALL TABLES IN SCHEMA public TO app;

CREATE FUNCTION trolls(t TIMESTAMP)
    RETURNS TABLE(
        member BIGINT, 
        upvotes BIGINT, 
        downvotes BIGINT, 
        active BOOLEAN
    ) AS $X$
    WITH Usr AS (
        SELECT id AS memberid, (t - last_activity) <= '365 days' AS active
        FROM Member
    ), Sums AS (
        SELECT Usr.memberid, SUM(downvotes) AS downvotes, SUM(upvotes) AS upvotes, active
        FROM Action JOIN Usr ON (Action.memberid = Usr.memberid)
        GROUP BY Usr.memberid, Usr.active
    )
    SELECT memberid as member, upvotes::BIGINT, downvotes::BIGINT, active
    FROM Sums
    WHERE downvotes > upvotes
    ORDER BY downvotes - upvotes DESC, member ASC
$X$ LANGUAGE SQL;
