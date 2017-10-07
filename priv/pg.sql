--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.3
-- Dumped by pg_dump version 9.6.4

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: current; Type: TABLE; Schema: public; Owner: nts
--

CREATE TABLE current (
    device character varying(32) NOT NULL,
    dtm timestamp without time zone,
    coords point,
    data text
);


ALTER TABLE current OWNER TO nts;

--
-- Name: device; Type: TABLE; Schema: public; Owner: nts
--

CREATE TABLE device (
    id integer NOT NULL,
    devid character varying(127),
    devtype character varying(127),
    label character varying(127),
    config text
);


ALTER TABLE device OWNER TO nts;

--
-- Name: device_01; Type: TABLE; Schema: public; Owner: nts
--

CREATE TABLE device_01 (
    id bigint NOT NULL,
    dtm timestamp without time zone,
    coords point,
    data text,
    hex boolean,
    frame text,
    received timestamp without time zone,
    internal text
);


ALTER TABLE device_01 OWNER TO nts;

--
-- Name: device_01_id_seq; Type: SEQUENCE; Schema: public; Owner: nts
--

CREATE SEQUENCE device_01_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE device_01_id_seq OWNER TO nts;

--
-- Name: device_01_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: nts
--

ALTER SEQUENCE device_01_id_seq OWNED BY device_01.id;


--
-- Name: device_id_seq; Type: SEQUENCE; Schema: public; Owner: nts
--

CREATE SEQUENCE device_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE device_id_seq OWNER TO nts;

--
-- Name: device_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: nts
--

ALTER SEQUENCE device_id_seq OWNED BY device.id;


--
-- Name: events; Type: TABLE; Schema: public; Owner: nts
--

CREATE TABLE events (
    id bigint NOT NULL,
    device character varying(32) NOT NULL,
    dtm timestamp without time zone,
    coords point,
    type character varying(127) NOT NULL,
    data text
);


ALTER TABLE events OWNER TO nts;

--
-- Name: events_id_seq; Type: SEQUENCE; Schema: public; Owner: nts
--

CREATE SEQUENCE events_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE events_id_seq OWNER TO nts;

--
-- Name: events_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: nts
--

ALTER SEQUENCE events_id_seq OWNED BY events.id;


--
-- Name: device id; Type: DEFAULT; Schema: public; Owner: nts
--

ALTER TABLE ONLY device ALTER COLUMN id SET DEFAULT nextval('device_id_seq'::regclass);


--
-- Name: device_01 id; Type: DEFAULT; Schema: public; Owner: nts
--

ALTER TABLE ONLY device_01 ALTER COLUMN id SET DEFAULT nextval('device_01_id_seq'::regclass);


--
-- Name: events id; Type: DEFAULT; Schema: public; Owner: nts
--

ALTER TABLE ONLY events ALTER COLUMN id SET DEFAULT nextval('events_id_seq'::regclass);


--
-- Name: current current_pkey; Type: CONSTRAINT; Schema: public; Owner: nts
--

ALTER TABLE ONLY current
    ADD CONSTRAINT current_pkey PRIMARY KEY (device);


--
-- Name: device device_devid_key; Type: CONSTRAINT; Schema: public; Owner: nts
--

ALTER TABLE ONLY device
    ADD CONSTRAINT device_devid_key UNIQUE (devid);


--
-- Name: device device_pkey; Type: CONSTRAINT; Schema: public; Owner: nts
--

ALTER TABLE ONLY device
    ADD CONSTRAINT device_pkey PRIMARY KEY (id);


--
-- Name: dtm; Type: INDEX; Schema: public; Owner: nts
--

CREATE INDEX dtm ON device_01 USING btree (dtm);


--
-- Name: evt_dev; Type: INDEX; Schema: public; Owner: nts
--

CREATE INDEX evt_dev ON events USING btree (device);


--
-- Name: evt_dtm; Type: INDEX; Schema: public; Owner: nts
--

CREATE INDEX evt_dtm ON events USING btree (dtm);


--
-- Name: rec; Type: INDEX; Schema: public; Owner: nts
--

CREATE INDEX rec ON device_01 USING btree (received);


--
-- PostgreSQL database dump complete
--

