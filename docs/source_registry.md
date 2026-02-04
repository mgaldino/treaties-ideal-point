# Source Registry (Open Access)

Access date for all sources: 2026-01-31
Tariff sources added: 2026-02-02
New issue-area sources added: 2026-02-03

## Issue-area taxonomy (derived from sources)
- Trade agreements (PTAs/RTAs)
- Investment treaties (BITs, IIAs, investment chapters)
- Security alliances
- Environment agreements (IEAs)
- Human rights treaties (core UN HR instruments + optional protocols)
- Arms control / disarmament treaties (Chapter XXVI + related)
- Intellectual property treaties (WIPO-administered)

## Fields recorded per source
- Issue area
- Coverage and scope
- Organization of data (structure)
- Access and format
- Regional flag (for robustness)
- Suitability rationale (ideal-point signal)
- Full citation path (URL + suggested citation when provided)

---

## Trade agreements (PTAs/RTAs)

### DESTA v2.3 (Design of Trade Agreements)
- Issue area: Trade agreements (PTAs)
- Coverage and scope: PTAs, design features; time period noted as 1948-2019; >710 agreements; current version September 2025.
- Organization of data: treaty list and dyadic treaty list (CSV/XLSX); content coding; indices; codebooks.
- Access and format: public downloads in CSV/XLSX from DESTA Downloads page.
- Regional flag: YES (PTAs include regional and bilateral agreements).
- Suitability rationale: rich treaty design features + membership allow construction of country-year participation and agreement-feature vectors for ideal-point estimation.
- Full citation path:
  - Dataset page: https://www.designoftradeagreements.org/downloads/
  - Project description: https://www.designoftradeagreements.org/project-description/
  - Recommended citation (from downloads page): Duer, Andreas; Baccini, Leonardo; Elsig, Manfred (2014). "The Design of International Trade Agreements: Introducing a New Database." The Review of International Organizations 9(3): 353-375. Version 2.3 (2025).

### WTO Regional Trade Agreements (RTA) Database
- Issue area: Trade agreements (RTAs)
- Coverage and scope: RTAs notified by WTO members; global coverage; temporal coverage listed as 1996-present.
- Organization of data: agreement profiles (name, members, signature/notification/entry-into-force dates, coverage/type); provisions; downloadable lists.
- Access and format: downloadable in Excel and CSV (per WTO Data Portal entry); web portal.
- Regional flag: YES (RTA-focused by definition).
- Suitability rationale: provides official notification-based timeline for trade agreements; useful to build country-year participation and timing of trade alignments.
- Full citation path:
  - Dataset page: https://data.wto.org/en/dataset/ext_rta
  - Portal link: https://rtais.wto.org/


### WTO Accession (membership dates)
- Issue area: Trade (membership/accession)
- Coverage and scope: WTO membership accession dates for members (global); accession dates used as a standalone trade item.
- Organization of data: membership table (country + date of accession).
- Access and format: open web table (HTML).
- Regional flag: NO (membership is global, not regional).
- Suitability rationale: accession to WTO is a strong signal of commitment to the liberal trade order; useful as an anchor item.
- Full citation path:
  - WTO accession status page: https://www.wto.org/english/thewto_e/acc_e/acc_status_e.htm
  - Wikipedia table (used for extraction): https://en.wikipedia.org/wiki/Member_states_of_the_World_Trade_Organization
### World Bank WITS Global Preferential Trade Agreements Database (GPTAD)
- Issue area: Trade agreements (PTAs)
- Coverage and scope: PTAs worldwide, including those not notified to the WTO; >330 PTAs; includes original texts; searchable by provisions; integrated with WITS.
- Organization of data: searchable database; library with agreement texts; agreement metadata (membership, dates, in-force status) available via the library.
- Access and format: open access per World Bank Data Catalog; web portal search and library; license CC BY 4.0.
- Regional flag: YES (includes regional/plurilateral PTAs).
- Suitability rationale: provides treaty texts and metadata for constructing participation and text-based alignment measures.
- Full citation path:
  - Data catalog entry: https://datacatalog.worldbank.org/search/dataset/0041745/wits-global-preferential-trade-agreement-database
  - GPTAD portal: https://wits.worldbank.org/gptad.html

---

## Tariffs (MFN and Applied)

### WITS/UNCTAD TRAINS Tariff Database (via WITS API)
- Issue area: Tariffs (MFN applied and preferential/applied rates)
- Coverage and scope: country-reporter tariff schedules and applied preferential rates; HS nomenclature varies by year.
- Organization of data: SDMX-JSON via WITS API; partner-specific tariff data.
- Access and format: open access with free WITS account; API endpoints.
- Suitability rationale: provides MFN applied and applied preferential tariff rates needed for preferential margin construction.
- Full citation path:
  - WITS API guide: https://wits.worldbank.org/data/public/WITSAPI_UserGuide.pdf
  - WITS API base: https://wits.worldbank.org/API/V1

### WITS Product Concordance (HS mappings)
- Issue area: HS nomenclature concordances
- Coverage and scope: concordance tables between HS revisions (e.g., HS2012 to HS1988/92).
- Organization of data: downloadable ZIP with CSV mappings.
- Access and format: public download via WITS product concordance page.
- Suitability rationale: enables mapping historical HS codes to a single target revision for consistent product panels.
- Full citation path:
  - Product concordance page: https://wits.worldbank.org/product_concordance.html
  - Example concordance file: https://wits.worldbank.org/data/public/concordance/Concordance_H4_to_H0.zip

---

## Investment treaties

### UNCTAD International Investment Agreements (IIA) Navigator
- Issue area: Investment treaties (BITs and other IIAs)
- Coverage and scope: UNCTAD-maintained database of IIAs; browse by country or grouping; advanced treaty search.
- Organization of data: treaty records by country with dates and text (per third-party descriptor); searchable web interface.
- Access and format: web database; bulk export not clearly documented (likely requires scraping or manual extraction).
- Regional flag: MIXED (includes BITs, multilateral and regional investment agreements).
- Suitability rationale: official UNCTAD registry of investment treaties; essential for global investment-alignment dimension.
- Full citation path:
  - IIA Navigator: https://investmentpolicy.unctad.org/international-investment-agreements/
  - Country grouping entry: https://investmentpolicy.unctad.org/international-investment-agreements/by-country-grouping

### EDIT (Electronic Database of Investment Treaties)
- Issue area: Investment treaties
- Coverage and scope: comprehensive full-text IIAs; includes BITs, investment chapters of PTAs, RIAs, FCNs; texts standardized in English, machine-readable XML.
- Organization of data: searchable database; treaty texts and metadata; supports text-as-data analysis.
- Access and format: web database; machine-readable XML (per EDIT documentation).
- Regional flag: YES (includes RIAs and PTAs with investment chapters).
- Suitability rationale: text-as-data capabilities enable richer ideal-point signals beyond membership dates.
- Full citation path:
  - EDIT home: https://edit.wti.org/
  - About page: https://edit.wti.org/document/about
  - Recommended citation (site): Alschner, Wolfgang; Elsig, Manfred; Polanco, Rodrigo (2021). "Introducing the Electronic Database of Investment Treaties (EDIT): The Genesis of a New Database and Its Use." World Trade Review 20(1): 73-94.

---

## Security alliances

### ATOP 5.1 (Alliance Treaty Obligations and Provisions)
- Issue area: Security alliances
- Coverage and scope: military alliance agreements signed by all countries worldwide, 1815-2018.
- Organization of data: alliance treaty content and obligations; downloadable datasets (CSV/STATA) from data page.
- Access and format: data page provides CSV and STATA downloads; codebook available.
- Regional flag: MIXED (global scope; includes regional and extra-regional alliances).
- Suitability rationale: alliance commitments provide a strong security-alignment signal over time.
- Full citation path:
  - ATOP home: https://www.atopdata.org/
  - Data page: linked from site home (CSV/STATA downloads).

---

## Environment agreements

### IEADB (International Environmental Agreements Database Project)
- Issue area: Environment agreements (MEAs/BEAs)
- Coverage and scope: global IEAs; includes agreements, country participation, and treaty texts; data updated weekly.
- Organization of data: CSV files with separate tables for treaties, members (signature/ratification/entry-into-force dates), countries, and treaty texts; semicolon delimiters.
- Access and format: downloadable ZIP/CSV and text files from IEADB download page.
- Regional flag: MIXED (includes MEAs and BEAs; BEAs may be regional or bilateral).
- Suitability rationale: explicit country-by-treaty dates allow construction of country-year participation matrices for environmental alignment.
- Full citation path:
  - Download page: https://www.iea.ulaval.ca/en/download/ieadb-dataset-current-version-all-ieadb-dataset-files
  - Project home: https://www.iea.ulaval.ca/
  - Required citation (site): Mitchell, Ronald B. et al. (2020). "What We Know (and Could Know) About International Environmental Agreements." Global Environmental Politics 20(1): 103-121.

---

## Human rights treaties

### UN Treaty Collection — Chapter IV (Human Rights)
- Issue area: Human rights (treaty ratification)
- Coverage and scope: 9 core UN human rights treaties + optional protocols (18 instruments); ratification/accession/succession dates for ~200 countries; temporal coverage from treaty opening through present.
- Organization of data: participant tables per treaty on treaties.un.org ViewDetails pages; scraped to CSV.
- Access and format: open access; HTML scraping from treaties.un.org Chapter IV.
- Regional flag: NO (global scope).
- Suitability rationale: ratification patterns across core HR treaties provide a strong signal of country commitment to the international human rights regime; wide variation in participation across treaties and protocols.
- Full citation path:
  - Chapter IV index: https://treaties.un.org/Pages/Treaties.aspx?id=4&subid=A&clang=_en
  - Individual treaties: ViewDetails.aspx pages (IV-2 through IV-16)
- Treaties covered: CERD, ICESCR (+OP), ICCPR (+OP1, +OP2), CEDAW (+OP), CAT (+OPCAT), CRC (+OP-AC, +OP-SC, +OP-IC), CMW, CRPD (+OP), CED
- Acquisition date: 2026-02-03
- Acquisition script: `scripts/python/acquire_un_hr_treaties.py`

---

## Arms control / disarmament treaties

### UN Treaty Collection — Chapter XXVI (Disarmament)
- Issue area: Arms control / disarmament (treaty ratification)
- Coverage and scope: 12 instruments from Chapter XXVI (Disarmament); ratification/accession/succession dates for ~197 countries; temporal coverage from treaty opening through present.
- Organization of data: participant tables per treaty on treaties.un.org ViewDetails pages; scraped to CSV.
- Access and format: open access; HTML scraping from treaties.un.org Chapter XXVI.
- Regional flag: NO (global scope).
- Suitability rationale: ratification patterns across disarmament treaties reveal country commitment to arms control norms; strong variation between signatories/non-signatories on key instruments (CTBT, Ottawa, TPNW).
- Full citation path:
  - Chapter XXVI index: https://treaties.un.org/Pages/Treaties.aspx?id=26&subid=A&clang=_en
  - Individual treaties: ViewDetails.aspx pages (XXVI-1 through XXVI-9)
- Treaties covered: ENMOD, CCW (+P4, +P2a, +Art1 amendment, +P5), CWC, CTBT, Ottawa Treaty, CCM, ATT, TPNW
- Note: NPT (1968) and BWC (1972) are NOT deposited with the UN Secretary-General and are therefore not in this source. Could be added from IAEA/BWC ISU separately.
- Acquisition date: 2026-02-03
- Acquisition script: `scripts/python/acquire_arms_control.py`

---

## Intellectual property treaties

### WIPO Lex — WIPO-Administered Treaties
- Issue area: Intellectual property (treaty ratification)
- Coverage and scope: 26 WIPO-administered treaties; contracting party dates for ~199 countries; temporal coverage from treaty opening (some from 1883) through present.
- Organization of data: contracting parties tables per treaty on WIPO Lex portal; scraped to CSV.
- Access and format: open access; HTML scraping from www.wipo.int/wipolex.
- Regional flag: NO (global scope).
- Suitability rationale: participation in WIPO treaties captures country engagement with the global IP regime; variation spans from universal participation (WIPO Convention, 194 members) to narrow membership (Washington Treaty, 10 members).
- Full citation path:
  - WIPO treaties portal: https://www.wipo.int/treaties/en/
  - WIPO Lex contracting parties: https://www.wipo.int/wipolex/en/treaties/ShowResults?search_what=C&treaty_id={id}
- Treaties covered:
  - Protection: WIPO Convention, Paris, Berne, Madrid (Indications), Madrid (Marks), Madrid Protocol, Hague, Lisbon, PCT, Budapest, Nairobi, TLT, PLT, Singapore
  - Copyright: WCT, WPPT, Rome, Phonograms, Brussels, Beijing, Marrakesh
  - Classification: Strasbourg, Nice, Vienna, Locarno
  - Other: Washington
- Acquisition date: 2026-02-03
- Acquisition script: `scripts/python/acquire_wipo_treaties.py`

---

## Notes on regional flagging
- For datasets that include RTAs/PTAs or RIAs, the "Regional flag" is set to YES or MIXED. In analysis, we can include these observations by default and optionally exclude them in robustness checks.

## Next planned actions (if approved)
- Shortlist a core subset for immediate ingestion (likely: DESTA, WTO RTA, UNCTAD IIA, EDIT, ATOP, IEADB).
- Create a reproducible acquisition log and scripts for each source.
