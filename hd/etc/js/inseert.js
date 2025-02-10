// INSEE Tool Core Module

const FIELD_IDS = {
    surname: { displayId: 'surname-line', inputId: 'surname' },
    firstname: { displayId: 'firstname-line', inputId: 'first_name' },
    'birth-date': { displayId: 'birth-date-line', inputPrefix: 'e_date' },
    'birth-place': { displayId: 'birth-place-line', inputPrefix: 'e_place' },
    'death-date': { displayId: 'death-date-line', inputPrefix: 'e_date' },
    'death-place': { displayId: 'death-place-line', inputPrefix: 'e_place' },
    'death-source': { displayId: 'death-source-line', inputPrefix: 'e_src' }
};

const InseeTools = {
  // Storage manager - simple key-value storage for INSEE data
  storage: {
      save: function(key, data) {
          localStorage.setItem(key, JSON.stringify(data));
      },

      load: function(key) {
          const data = localStorage.getItem(key);
          return data ? JSON.parse(data) : null;
      }
  },

  // Results page handler - captures INSEE data when a link is clicked
  results: {
      init: function() {
        // Store reference to results object
        const self = this;
        document.addEventListener('click', function(event) {
            if (event.target.closest('a[href*="m=S&edit=1"]')) {
                const link = event.target.closest('a');
                const key = link.textContent;
                const inseeData = self.captureMatchData(link);
                console.log('Captured data:', inseeData);  // Debug
                InseeTools.storage.save(key, inseeData);
            }
        });
      },

      captureMatchData: function(linkElement) {
          // First, capture the IdInsee line that appears before the link
          let idInseeLine = '';
          let currentNode = linkElement;

          // Walk backwards to find the IdInsee line
          while (currentNode && currentNode.previousSibling) {
              currentNode = currentNode.previousSibling;
              if (currentNode.nodeType === Node.TEXT_NODE &&
                  currentNode.textContent.trim().includes('IdInsee')) {
                  idInseeLine = currentNode.textContent.trim();
                  break;
              }
          }

          // Now capture everything after the link
          let dataBlock = '';
          currentNode = linkElement;

          // First add the link text itself
          dataBlock += linkElement.outerHTML + '\n';

          // Then continue with next siblings
          currentNode = linkElement.nextSibling;
          while (currentNode && !currentNode.matches?.('a')) {
              if (currentNode.nodeType === Node.TEXT_NODE) {
                  dataBlock += currentNode.textContent + '\n';
              } else if (currentNode.nodeType === Node.ELEMENT_NODE) {
                  // For HTML elements, get their text content
                  dataBlock += currentNode.textContent + '\n';
              }
              currentNode = currentNode.nextSibling;
          }

          // Combine the IdInsee line with the rest of the data
          const fullDataBlock = idInseeLine + '\n' + dataBlock;
          return this.parseDataBlock(fullDataBlock);
      },

      parseDataBlock: function(block) {
          const data = {
              title: { todonom: '', todo: '', resultnom: '', result: '', resultori: '', score: '' },
              blacklist: { idinsee: '', gwkey: '', todokey: '' },
              names: { surname_changes:'', surname: '', firstname_changes:'', firstname: '' },
              birth: { date_changes: '', day: '', month: '', year: '', place_changes: '', place: '' },
              death: { date_changes: '', day: '', month: '', year: '', place_changes: '', place: '' },
              source: ''
          };

          const lines = block.split('\n').filter(line => line);
          const idinseeMatch = lines[0].match(/(IdInsee[^)]*).*Score (.*) Matche/);
          data.blacklist.idinsee = idinseeMatch ? `${idinseeMatch[1]}` : '';
          data.title.score= idinseeMatch ? `${idinseeMatch[2]}` : '';

          const gwkeyMatch = lines[1].match(/>([^<]+)<\/a>|^([^<]+)$/);
          data.blacklist.gwkey = gwkeyMatch ? (gwkeyMatch[1] || gwkeyMatch[2]) : '';

          const todokeyMatch = lines[2].match(/^((.*?\|.*?)\|.*?\|(.*))$/);
          data.blacklist.todokey = todokeyMatch ? todokeyMatch[1] : '';
          data.title.todonom = todokeyMatch ? todokeyMatch[2] : '';
          data.title.todo = todokeyMatch ? todokeyMatch[3] : '';

        const resultMatch = lines[3].match(/^(.*\|.*)\|.\|(.*\|.*\|.*\|.*)\|(.*\|.*\|.*\|.*)\|/);
          data.title.resultnom = resultMatch ? resultMatch[1] : '';
          data.title.result = resultMatch ? resultMatch[2] : '';
          data.title.resultori = resultMatch ? resultMatch[3] : '';

          for (const line of lines.slice(3)) {
              if (line.includes('Nom')) {
                  const match = line.match(/(Nom.*(?:!=2|!=|=~|->\s))(.*?)$/);
                  if (match) {
                      data.names.surname_changes = match[1];
                      data.names.surname = match[2].trim();
                  }
              }
              else if (line.includes(' Prénom')) {
                  const match = line.match(/(Prénom.*(?:!=2|!=|=~|->\s))(.*?)$/);
                  if (match) {
                      data.names.firstname_changes = match[1];
                      data.names.firstname = match[2].trim();
                  }
              }
              else if (line.includes(' Date naissance')) {
                  const match = line.match(/(Date naissance.*)(\d{2})\/(\d{2})\/(\d{4})$/);
                  if (match) {
                      [, data.birth.date_changes, data.birth.day, data.birth.monosmonth, data.birth.year] = match;
                  }
              }
              else if (line.includes(' Lieu naissance')) {
                  const match = line.match(/(Lieu naissance.*(?:!=2|!=|=~|->\s))(.*?)$/);
                  if (match) {
                      data.birth.place_changes = match[1];
                      data.birth.place = match[2].trim();
                  }
              }
              else if (line.includes(' Date décès')) {
                  const match = line.match(/(Date décès.*)(\d{2})\/(\d{2})\/(\d{4})$/);
                  if (match) {
                      [, data.death.date_changes, data.death.day, data.death.month, data.death.year] = match;
                  }
              }
              else if (line.startsWith(' Lieu décès')) {
                  const match = line.match(/(Lieu décès.*(?:!=2|!=|=~|->\s))(.*?)$/);
                  if (match) {
                      data.death.place_changes = match[1];
                      data.death.place = match[2].trim();
                  }
              }
              else if (line.startsWith('Insee')) {
                const match = line.match(/^(Insee.*)$/);
                if (match) {
                    data.source = match[0];
                }
            }
          }

          return data;
      }
  },

  individual: {
      init: function() {
          const modLink = document.querySelector('#mod_ind');
          if (modLink) {
              const urlParams = new URLSearchParams(window.location.search);
              const personKey = urlParams.get('n');
              if (personKey) {
                  const baseHref = modLink.getAttribute('href');
                  const newHref = baseHref + '&key=' + encodeURIComponent(personKey);
                  window.location.href = newHref;
              }
          }
      }
  },

  // Form page handler - displays stored INSEE data and enables corrections
  form: {
      init: function() {
          const urlParams = new URLSearchParams(window.location.search);
          const key = urlParams.get('key');
          const inseeData = InseeTools.storage.load(key);

          if (key && inseeData) {
            console.log('Loaded data:', inseeData); // Debug
            this.createDataDisplay(inseeData);
            this.setupFieldFilling(inseeData);
          }
      },

      // Find all relevant fields under a section (birth/death)
      findEventFields: function(sectionId, skipDebug = false) {
          if (!skipDebug) {
              console.group(`Finding fields for ${sectionId}`);
          }

          const fields = {
              place: null,
              dateDay: null,
              dateMonth: null,
              dateYear: null,
              source: null,
              eventNum: null
          };

          // Recherche de la section
          const header = document.querySelector(`h3#${sectionId}`);
          if (!header) {
              console.warn(`No header found for ${sectionId}`);
              return null;
          }

          const card = header.closest('.card');
          if (!card) return null;

          const cardBody = card.querySelector('.card-body');
          if (!cardBody) return null;

          // Recherche du numéro d'événement
          const eventInput = cardBody.querySelector('input[id^="e_name"]');
          if (!eventInput) return null;

          const eventNum = eventInput.id.match(/\d+/)?.[0];
          if (!eventNum) return null;

          // Remplissage des champs
          fields.eventNum = eventNum;
          fields.place = cardBody.querySelector(`input[id="e_place${eventNum}"]`);
          fields.dateDay = cardBody.querySelector(`input[name="e_date${eventNum}_dd"]`);
          fields.dateMonth = cardBody.querySelector(`input[name="e_date${eventNum}_mm"]`);
          fields.dateYear = cardBody.querySelector(`input[name="e_date${eventNum}_yyyy"]`);
          fields.source = cardBody.querySelector(`input[id="e_src${eventNum}"], textarea[id="e_src${eventNum}"]`);

          fields.validate = function() {
              const requiredFields = [this.place, this.dateDay, this.dateMonth, this.dateYear];
              return requiredFields.every(f => f !== null);
          };

          if (!skipDebug) {
              console.log('Found fields:', fields);
              console.groupEnd();
          }

          return fields;
      },

      debugField: function(element, fieldName) {
          if (!element) {
              console.log(`${fieldName} not found`);
              return;
          }
          console.log(`${fieldName} found:`, {
              id: element.id,
              name: element.name,
              type: element.type,
              value: element.value
          });
      },

      debugEventFields: function() {
          ['birth', 'death'].forEach(section => {
              console.group(`${section} section debug:`);
              const fields = this.findEventFields(section);
              if (fields) {
                  this.debugField(fields.place, 'Place field');
                  this.debugField(fields.dateDay, 'Day field');
                  this.debugField(fields.dateMonth, 'Month field');
                  this.debugField(fields.dateYear, 'Year field');
                  this.debugField(fields.source, 'Source field');
                  console.log('Event number:', fields.eventNum);
              }
              console.groupEnd();
          });
      },

      autoFillIfEmpty: function(fieldId, value, callback) {
          // Extract the field if passed by id or by full reference
          const field = typeof fieldId === 'string' ?
              document.getElementById(fieldId) : fieldId;

          if (field && !field.value && value) {
              field.value = value;
              // Get the base id to find matching summary line
              const baseId = field.id || field.name.replace(/_[^_]+$/, '');
              this.addCheckToSummary(baseId);
              if (callback) callback();
          }
      },

      setupFieldFilling: function(data) {
          // Stockage des données pour utilisation ultérieure
          this.inseeData = data;

          let updatedFields = {
              'firstname': false,
              'surname': false,
              'birth-date': false,
              'birth-place': false,
              'death-date': false,
              'death-place': false,
              'death-source': false
          };
      },

      handleDateFields: function(prefix, date, type, onlyEmpty) {
          const dateFields = {
              dd: document.getElementById(`${prefix}_dd`),
              mm: document.getElementById(`${prefix}_mm`),
              yyyy: document.getElementById(`${prefix}_yyyy`)
          };

          const dateValues = {
              dd: date.day,
              mm: date.month,
              yyyy: date.year
          };

          // Si la date est partiellement vide OU si on remplace tout
          if (!onlyEmpty || this.isDatePartiallyEmpty(dateFields)) {
              let anyFieldUpdated = false;

              // Remplir tous les champs vides ou différents
              Object.entries(dateFields).forEach(([part, field]) => {
                  if (field && (!field.value || !onlyEmpty)) {
                      const newValue = dateValues[part];
                      if (field.value !== newValue) {
                          field.value = newValue;
                          anyFieldUpdated = true;
                      }
                  }
              });

              // Vérifier si la date est maintenant complète et correspond
              if (this.areDatesEqual(dateFields, dateValues)) {
                  this.updateVisualFeedback(type);
              }
          }
      },

      handleField: function(id, value, type, onlyEmpty) {
          if (value && (!onlyEmpty || !document.getElementById(id)?.value)) {
              this.handleDataReplacement(id, value, type);
          }
      },

      fillFields: function(onlyEmpty = false) {
          console.log('Starting fillFields, onlyEmpty:', onlyEmpty);
          if (!this.inseeData) {
              console.warn('No INSEE data available');
              return;
          }

          // Identity fields
          this.handleField('first_name', this.inseeData.names.firstname, 'firstname', onlyEmpty);
          this.handleField('surname', this.inseeData.names.surname, 'surname', onlyEmpty);

          // Birth fields
          const birthFields = this.findEventFields('birth');
          if (birthFields?.validate()) {
          this.handleDateFields(
              `e_date${birthFields.eventNum}`,
              this.inseeData.birth,
              'birth-date',
              onlyEmpty
          );
          this.handleField(`e_place${birthFields.eventNum}`, this.inseeData.birth.place, 'birth-place');
      }

          // Death fields similaires
          const deathFields = this.findEventFields('death');
              if (deathFields?.validate()) {
                  this.handleDateFields(
                      `e_date${deathFields.eventNum}`,
                      this.inseeData.death,
                      'death-date',
                      onlyEmpty
                  );
                  this.handleField(`e_place${deathFields.eventNum}`, this.inseeData.death.place, 'death-place');
                  this.handleField(`e_src${deathFields.eventNum}`, this.inseeData.source, 'death-source');
              }
      },

      fillEmptyFields: function() {
          this.setDeathStatus();
          this.fillFields(true);
          this.updateButtonStates('empty');
      },

      fillAllFields: function() {
          this.setDeathStatus();
          this.fillFields(false);
          this.updateButtonStates('all');
      },

      setDeathStatus: function() {
          const deathSelect = document.querySelector('select[id^="death_select"]');
          if (deathSelect) {
              // Extraire le numéro depuis l'id (ex: "death_select2" -> "2")
              const eventNum = deathSelect.id.match(/\d+/)?.[0];
              if (eventNum) {
                  deathSelect.value = 'Death';
                  // Appeler change_death avec le bon numéro
                  change_death(eventNum);
              }
          }
      },

      handleNameCase: function(value, fieldId) {
          const field = document.getElementById(fieldId);
          if (!field) return value;

          // Pour les noms de famille
          if (fieldId === 'surname') {
              const currentValue = field.value;
              // Si le champ est vide ou en majuscules, on garde la casse originale
              if (!currentValue || currentValue === currentValue.toUpperCase()) {
                  return value;
              }
              // Sinon on met en minuscules avec première lettre majuscule
              return value.toLowerCase().replace(/(?:^|\s)\S/g, c => c.toUpperCase());
          }

          // Pour les prénoms et autres champs avec texte
          if (fieldId === 'first_name' || field.value.length > 4) {
              const currentValue = field.value;
              const isCapitalized = /^[A-Z][a-z]/.test(currentValue);
              if (isCapitalized) {
                  // Si le champ existant utilise déjà la casse mixte
                  return value.toLowerCase().replace(/(?:^|\s|-)\S/g, c => c.toUpperCase());
              }
          }

          return value;
      },

      areDatesEqual: function(date1Fields, date2Fields) {
          return ['dd', 'mm', 'yyyy'].every(part => {
              const field1 = date1Fields[part];
              const value2 = date2Fields[part];
              return !field1?.value || !value2 || field1.value === value2;
          });
      },

      isDatePartiallyEmpty: function(dateFields) {
          return ['dd', 'mm', 'yyyy'].some(part => {
              const field = dateFields[part];
              return !field?.value;
          });
      },

      handleDataReplacement: function(targetId, value, fieldType) {
          const field = document.getElementById(targetId);
          if (!field) {
              console.warn('Field not found:', targetId);
              return;
          }

          // Traitement du champ
          if (fieldType === 'death-source') {
              if (field.value && !field.value.includes('Insee')) {
                  field.value = field.value + ' ; ' + value;
              } else {
                  field.value = value;
              }
              this.updateVisualFeedback(fieldType);
          } else if (fieldType.endsWith('-date')) {
              // Pour les dates, on ne met à jour l'UI que si tous les champs sont remplis
              const eventNum = targetId.match(/\d+/)?.[0];
              if (eventNum) {
                  const prefix = targetId.split('_').slice(0, -1).join('_');
                  const dateFields = {
                      dd: document.getElementById(`${prefix}_dd`),
                      mm: document.getElementById(`${prefix}_mm`),
                      yyyy: document.getElementById(`${prefix}_yyyy`)
                  };

                  field.value = value;

                  // Vérifier si la date est complète et correspond aux données INSEE
                  const dateType = fieldType.startsWith('birth') ? 'birth' : 'death';
                  const dateValues = {
                      dd: this.inseeData[dateType].day,
                      mm: this.inseeData[dateType].month,
                      yyyy: this.inseeData[dateType].year
                  };

                  const isComplete = Object.values(dateFields).every(f => f && f.value);
                  const isMatching = Object.entries(dateFields).every(([part, f]) =>
                      f && f.value === dateValues[part]
                  );

                  if (isComplete && isMatching) {
                      this.updateVisualFeedback(fieldType);
                  }
              }
          } else {
              field.value = this.handleNameCase(value, targetId);
              this.updateVisualFeedback(fieldType);
          }

          this.checkAllValidated();
      },

      updateVisualFeedback: function(fieldType) {
          const fieldConfig = FIELD_IDS[fieldType];
          if (fieldConfig) {
              const line = document.getElementById(fieldConfig.displayId);
              if (line) {
                  // Update icon
                  const icon = line.querySelector('.check-icon-container i');
                  if (icon) {
                      icon.className = 'fa fa-check mr-1 text-success';
                  }
                  // Update value selection
                  const valueSpan = line.querySelector('.replacement-value');
                  if (valueSpan) {
                      valueSpan.classList.remove('user-select-all');
                      valueSpan.classList.add('user-select-none');
                      valueSpan.style.cursor = 'default';
                      valueSpan.removeAttribute('title');
                  }
              }
          }
      },

      // Setup click handlers for data replacement
      setupDataReplacementHandlers: function() {
          document.querySelectorAll('.replacement-value').forEach(span => {
              span.addEventListener('click', (e) => {
                  const fieldType = e.target.dataset.fieldType;
                  const fieldConfig = FIELD_IDS[fieldType];

                  if (!fieldConfig) {
                      console.warn('No field configuration for:', fieldType);
                      return;
                  }

                  // Déterminer l'ID du champ cible
                  let targetId;
                  if (fieldConfig.inputId) {
                      targetId = fieldConfig.inputId;
                  } else if (fieldConfig.inputPrefix) {
                      const eventFields = this.findEventFields(fieldType.split('-')[0], true);
                      if (eventFields?.eventNum) {
                          targetId = `${fieldConfig.inputPrefix}${eventFields.eventNum}`;
                      }
                  }

                  if (!targetId) {
                      console.warn('Could not determine target ID for:', fieldType);
                      return;
                  }

                  const value = e.target.textContent;
                  this.handleDataReplacement(targetId, value, fieldType);
              });
          });
      },

      checkAllValidated: function() {
          const allLines = document.querySelectorAll('.data-line .check-icon-container i');
          const allChecked = Array.from(allLines).slice(1).every(icon =>
              icon.classList.contains('fa-check')
          );

          if (allChecked) {
              const titleIcon = document.querySelector('.data-line:first-child .check-icon-container i');
              if (titleIcon && !titleIcon.classList.contains('fa-check')) {
                  titleIcon.className = 'fa fa-check fa-xl text-success';
              }
          }
      },

      updateButtonStates: function(fillType) {
          const emptyBtn = document.getElementById('btn-fill-empty');
          const allBtn = document.getElementById('btn-fill-all');

          const disableButton = (btn) => {
              if (btn) {
                  btn.disabled = true;
                  btn.style.opacity = '0.5';
                  btn.classList.add('disabled');
              }
          };

          if (fillType === 'empty') {
              disableButton(emptyBtn);
          } else if (fillType === 'all') {
              disableButton(emptyBtn);
              disableButton(allBtn);
          }
      },

      markNonMatch: function() {
          if (this.inseeData) {
              this.inseeData.blacklist.idinsee = '%' + this.inseeData.blacklist.idinsee;

              // Mise à jour visuelle des boutons
              const nonMatchBtn = document.getElementById('btn-blacklist-nonmatch');
              const matchBtn = document.getElementById('btn-blacklist-match');

              if (nonMatchBtn) {
                  nonMatchBtn.classList.remove('btn-outline-danger');
                  nonMatchBtn.classList.add('btn-danger');  // Bouton actif devient plein
              }
              if (matchBtn) {
                  matchBtn.classList.remove('btn-outline-success');
                  matchBtn.classList.add('btn-outline-light');  // L'autre devient light
                  matchBtn.disabled = true;
              }
              this.disableBlacklistButtons();
          }
      },

      markFullMatch: function() {
          if (this.inseeData) {
              this.inseeData.blacklist.idinsee = '$' + this.inseeData.blacklist.idinsee;

              // Mise à jour visuelle des boutons
              const nonMatchBtn = document.getElementById('btn-blacklist-nonmatch');
              const matchBtn = document.getElementById('btn-blacklist-match');

              if (matchBtn) {
                  matchBtn.classList.remove('btn-outline-success');
                  matchBtn.classList.add('btn-success');  // Bouton actif devient plein
              }
              if (nonMatchBtn) {
                  nonMatchBtn.classList.remove('btn-outline-danger');
                  nonMatchBtn.classList.add('btn-outline-light');  // L'autre devient light
                  nonMatchBtn.disabled = true;
              }
              this.disableBlacklistButtons();
          }
      },

      disableBlacklistButtons: function() {
          const nonMatchBtn = document.getElementById('btn-blacklist-nonmatch');
          const matchBtn = document.getElementById('btn-blacklist-match');

          [nonMatchBtn, matchBtn].forEach(btn => {
              if (btn) {
                  btn.disabled = true;
                  btn.style.opacity = '0.5';
                  btn.classList.add('disabled');
              }
          });
      },

      exportBlacklist: function() {
          if (!this.inseeData?.blacklist) return;

          const content = [
              this.inseeData.blacklist.idinsee,
              this.inseeData.blacklist.gwkey,
              this.inseeData.blacklist.todokey,
              '',  // Empty line at the end
          ].join('\n');

          // Create blob and trigger download
          const blob = new Blob([content], { type: 'text/plain' });
          const url = window.URL.createObjectURL(blob);
          const a = document.createElement('a');
          a.href = url;
          a.download = 'blacklist.txt';
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          window.URL.revokeObjectURL(url);
      },

      createRawDataInfo: function(data) {
          const displayDiv = document.createElement('div');
          displayDiv.className = 'insee-blacklist bg-light p-3 mb-2 border rounded';

          displayDiv.innerHTML = `
              <div class="d-flex">
                  <div class="flex-grow-1">
                      <h4 class="mb-3"><span class="user-select-none ">Données de référence : </span>${data.blacklist.gwkey}</h4>
                      <div class="pl-2">
                          <div class="col">${data.title.todonom}</div>
                          <div class="col">${data.title.resultnom}</div>
                          <div class="col">${data.title.todo}</div>
                          <div class="col">${data.title.result}</div>
                          <div class="col">(Codes Insee communes originaux ${data.title.resultori} — Score ${data.title.score})</div>
                      </div>
                  </div>
              </div>
          `;

          return displayDiv;
      },

      createDataDisplay: function(data) {
          const rawdataDiv = this.createRawDataInfo(data);

          const displayDiv = document.createElement('div');
          displayDiv.className = 'insee-data bg-light p-3 mt-2 mb-3 border rounded';

          const header = `
              <div class="data-line">
                  <div class="d-flex align-items-center">
                      <div class="flex-grow-1">
                          <h4 class=" user-select-none">Données manquantes :</h4>
                      </div>
                      <div class="check-icon-container">
                          <i class="fa fa-square-o text-muted"></i>
                      </div>
                  </div>
              </div>
          `;

          // Handle all data lines consistently, including source
          const dataLines = [
              ['surname', data.names.surname_changes, data.names.surname],
              ['firstname', data.names.firstname_changes, data.names.firstname],
              ['birth-date', data.birth.date_changes,
                  `${data.birth.day}/${data.birth.month}/${data.birth.year}`],
              ['birth-place', data.birth.place_changes, data.birth.place],
              ['death-date', data.death.date_changes,
                  `${data.death.day}/${data.death.month}/${data.death.year}`],
              ['death-place', data.death.place_changes, data.death.place],
              ['death-source', '', data.source]
          ].map(([type, changes, value]) => this.createDataLine(type, changes, value))
           .filter(line => line).join('\n');

          displayDiv.innerHTML = `
              <div class="d-flex">
                  <div class="flex-grow-1">
                     <div class="col-11 pl-0">
                         ${header}
                         ${dataLines}
                      </div>
                  </div>
                  <div class="d-flex flex-column align-self-center">
                      <button id="btn-fill-empty" class="btn btn-outline-info"
                          onclick="InseeTools.form.fillEmptyFields.call(InseeTools.form)"
                          title="Remplir tous les champs vides avec les données de l’Insee">
                          <i class="fa fa-clone"></i> Remplir vide
                      </button>
                      <button id="btn-fill-all" class="btn btn-outline-info my-2"
                          onclick="InseeTools.form.fillAllFields.call(InseeTools.form)"
                          title="Remplacer toutes les données par celles de l’Insee">
                          <i class="fa fa-paste"></i> Remplir tout
                      </button>
                      <button id="btn-validate" class="btn btn-primary"
                          onclick="document.forms['upd'].submit()">
                          <i class="fa fa-share"></i> Valider
                      </button>
                      <div class="flex-grow-1"></div>
                  </div>
                  <div class="d-flex flex-column align-self-center ml-3">
                      <button id="btn-blacklist-nonmatch" class="btn btn-outline-danger"
                          onclick="InseeTools.form.markNonMatch()"
                          title="Ajouter cette entrée Insee en liste noire pour cet individu">
                          <i class="fa fa-times"></i> Non-correspondance
                      </button>
                      <button id="btn-blacklist-match" class="btn btn-outline-success my-2"
                          onclick="InseeTools.form.markFullMatch()"
                          title="Ajouter cette entrée Insee en liste noire définitivement">
                          <i class="fa fa-check"></i> Correspondance
                      </button>
                      <button id="btn-blacklist-export" class="btn btn-outline-primary"
                          onclick="InseeTools.form.exportBlacklist()">
                          <i class="fa fa-download"></i> Exporter liste noire
                      </button>
                      <div class="flex-grow-1"></div>
                  </div>
              </div>
          `;

          // Insert after menubar, before form
          const form = document.querySelector('form[name="upd"]');
          form.parentNode.insertBefore(rawdataDiv, form);
          form.parentNode.insertBefore(displayDiv, form);

          // Setup click handlers
          this.setupDataReplacementHandlers();
      },

      addCheckToSummary: function(targetId) {
          const summaryLine = document.getElementById(`${targetId}-line`);
          if (summaryLine) {
              const iconContainer = summaryLine.querySelector('.check-icon-container');
              if (iconContainer && !iconContainer.querySelector('.fa-check')) {
                  iconContainer.innerHTML = '<i class="fa fa-check text-success"></i>';
              }
          }
      },

      createDataLine: function(fieldType, changes, value) {
          if (!value || (fieldType.endsWith('-date') && !value.match(/\d/))) return '';

          const fieldConfig = FIELD_IDS[fieldType];
          if (!fieldConfig) return '';

          const displayChanges = fieldType === 'death-source' ? 'Source décès : ' : (changes || '');

          const lineId = fieldConfig.displayId;

          return `
              <div class="data-line pl-4" id="${lineId}">
                  <div class="d-flex align-items-center">
                      <div class="flex-grow-1">
                          <span class="text-muted user-select-none">${displayChanges || ''}</span>
                          <span class="replacement-value user-select-all"
                                data-field-type="${fieldType}"
                                title="Remplacer cette donnée"
                                style="cursor: pointer">${value}</span>
                      </div>
                      <div class="check-icon-container">
                          <i class="fa fa-square-o text-muted"></i>
                      </div>
                  </div>
              </div>
          `;
      }
  }
}