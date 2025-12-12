// INSEE Tool Core Module

const FIELD_IDS = {
    surname: { displayId: 'surname-line', inputId: 'surname' },
    firstname: { displayId: 'firstname-line', inputId: 'first_name' },
    'birth-date': {
        displayId: 'birth-date-line',
        inputPrefix: 'e_date',
        isDate: true
    },
    'birth-place': { displayId: 'birth-place-line', inputPrefix: 'e_place' },
    'death-date': {
        displayId: 'death-date-line',
        inputPrefix: 'e_date',
        isDate: true
    },
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

          // Ajouter les boutons toggle une seule fois sur la page RESULT
          if (window.location.href.includes('f=RESULT')) {
              this.addToggleButtons();
          }

          document.addEventListener('click', function(event) {
              // Traitement des liens
              if (event.target.closest('a[href*="m=S&edit=1"]')) {
                  const link = event.target.closest('a');
                  const key = link.textContent;
                  const inseeData = self.captureMatchData(link);
                  console.log('Captured data:', inseeData);
                  InseeTools.storage.save(key, inseeData);

                  // Automatiquement marquer comme traité quand on clique sur le lien
                  const paragraph = link.closest('p.ins-block');
                  if (paragraph) {
                      paragraph.classList.add('ins-off');
                      const btn = paragraph.querySelector('.ins-btn');
                      if (btn) btn.innerHTML = '<i class="fa fa-xmark"></i>';
                  }
              }

              // Gestion des boutons toggle
              if (event.target.closest('.ins-btn')) {
                  event.preventDefault();
                  event.stopPropagation();

                  const btn = event.target.closest('.ins-btn');
                  const paragraph = btn.parentElement;

                  paragraph.classList.toggle('ins-off');
                  btn.innerHTML = paragraph.classList.contains('ins-off') ?
                      '<i class="fa fa-xmark"></i>' :
                      '<i class="fa fa-chevron-left"></i>';
              }
          });
      },

      addToggleButtons: function() {
          // Style unique et minimal
          const style = document.createElement('style');
          style.textContent = `
              .ins-btn {
                  position: absolute;
                  left: -30px;
                  height: 100%;
                  display: flex;
                  align-items: center;
                  background: #f8f9fa;
                  border: 1px solid #dee2e6;
                  cursor: pointer;
                  padding: 0 5px;
              }
              .ins-block {
                  position: relative;
                  margin-left: 5px;
              }
              .ins-off {
                  background-color: #f8f9fa;
              }
              .ins-off a {
                  color: #6c757d;
                  pointer-events: none;
                  text-decoration: none;
              }
          `;
          document.head.appendChild(style);

          // Traitement des paragraphes contenant IdInsee
          const paragraphs = Array.from(document.querySelectorAll('p'))
              .filter(p => p.textContent.includes('IdInsee'));

          paragraphs.forEach(p => {
              p.classList.add('ins-block');

              // Créer le bouton sans saut de ligne
              const btn = document.createElement('button');
              btn.className = 'ins-btn';
              btn.innerHTML = '<i class="fa fa-chevron-left"></i>';

              // Important: insérer le bouton sans créer de saut de ligne
              if (p.firstChild) {
                  // Si le premier enfant est un nœud texte, ajuster directement
                  if (p.firstChild.nodeType === Node.TEXT_NODE) {
                      const textContent = p.firstChild.textContent;
                      if (textContent.startsWith('\n')) {
                          p.firstChild.textContent = textContent.trimStart();
                      }
                  }
                  p.insertBefore(btn, p.firstChild);
              } else {
                  p.appendChild(btn);
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
              title: {
                  todo_fn: '', todo_sn: '', todo_bd: '', todo_bp: '', todo_dd: '', todo_dp: '',
                  result_fn: '', result_sn: '', result_bd: '', result_bp: '', result_dd: '', result_dp: '',
                  score: ''
              },
              blacklist: { idinsee: '', gwkey: '', todokey: '' },
              names: { surname_changes:'', surname: '', firstname_changes:'', firstname: '' },
              birth: { date_changes: '', day: '', month: '', year: '', place_changes: '', place: '', place_brut: '' },
              death: { date_changes: '', day: '', month: '', year: '', place_changes: '', place: '' },
              source: ''
          };

          const lines = block.split('\n').filter(line => line);
          const idinseeMatch = lines[0].match(/(IdInsee[^)]*).*Score (.*) Matche/);
          data.blacklist.idinsee = idinseeMatch ? `${idinseeMatch[1]}` : '';
          data.title.score= idinseeMatch ? `${idinseeMatch[2]}` : '';

          const gwkeyMatch = lines[1].match(/>([^<]+)<\/a>|^([^<]+)$/);
          data.blacklist.gwkey = gwkeyMatch ? (gwkeyMatch[1] || gwkeyMatch[2]) : '';

          const todoMatch = lines[2].match(/^((.*)\|(.*)\|.\|(.*)\|(.*)\|(.*)\|(.*))$/);
          data.blacklist.todokey = todoMatch ? todoMatch[1] : '';
          data.title.todo_sn = todoMatch ? todoMatch[2] : '';
          data.title.todo_fn = todoMatch ? todoMatch[3] : '';
          data.title.todo_bd = todoMatch ? todoMatch[4] : '';
          data.title.todo_bp = todoMatch ? todoMatch[5] : '';
          data.title.todo_dd = todoMatch ? todoMatch[6] : '';
          data.title.todo_dp = todoMatch ? todoMatch[7] : '';

        const resultMatch = lines[3].match(/^(.*)\|(.*)\|.\|(.*)\|(.*)\|(.*)\|(.*)\|.*\|.*\|.*\|.*\|.*$/);
          data.title.result_sn = resultMatch ? resultMatch[1] : '';
          data.title.result_fn = resultMatch ? resultMatch[2] : '';
          data.title.result_bd = resultMatch ? resultMatch[3] : '';
          data.title.result_bp = resultMatch ? resultMatch[4] : '';
          data.title.result_dd = resultMatch ? resultMatch[5] : '';
          data.title.result_dp = resultMatch ? resultMatch[6] : '';

          for (const line of lines.slice(3)) {
              if (line.includes('Nom')) {
                  const match = line.match(/(Nom.*(?:!=2|!=|=~|->\s))(.*?)$/);
                  if (match) {
                      data.names.surname_changes = match[1];
                      data.names.surname = match[2].trim();
                  }
              }
              else if (line.includes('Prénom')) {
                  const match = line.match(/(.+?Prénom.*(?:!=|->\s))(.*?)$/);
                  if (match) {
                      data.names.firstname_changes = match[1];
                      data.names.firstname = match[2].trim();
                  }
              }
              else if (line.includes(' Date naissance')) {
                  const match = line.match(/(Date naissance.*)(\d{2})\/(\d{2})\/(\d{4})$/);
                  if (match) {
                      [, data.birth.date_changes, data.birth.day, data.birth.month, data.birth.year] = match;
                  }
              }
              else if (line.includes(' Lieu naissance')) {
                  const match = line.match(/(Lieu naissance.*(?:!=2|!=|=~|->\s))(.*?)$/);
                  if (match) {
                      data.birth.place_changes = match[1];
                      data.birth.place = match[2].trim();
                  }
              }
              else if (line.includes(' Naissance Insee')) {
                  const match = line.match(/Naissance Insee :.(.*?)$/);
                  if (match) {
                      data.birth.place_brut = match[1];
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
              window.location.href = modLink.getAttribute('href');
          }
      }
  },

  // Form page handler - displays stored INSEE data and enables corrections
  form: {
      init: function() {
          const selfElement = document.querySelector('#self[data-key]');
          if (!selfElement) {
              console.log('No #self element with data-key found');
              return;
          }
          
          const key = selfElement.getAttribute('data-key');
          if (!key) {
              console.log('Empty data-key attribute');
              return;
          }
          
          // Check if this key exists in localStorage
          const inseeData = InseeTools.storage.load(key);
          if (!inseeData) {
              console.log('No data found in storage for key:', key);
              return;
          }
          
          // If we got here, we have valid data to display
          console.log('Loaded data for:', key);
          this.createDataDisplay(inseeData);
          this.setupFieldFilling(inseeData);
      },

      // Fonction de vérification pour les prénoms alias
      // Retourne le numéro de l'alias en conflit, ou false si pas de conflit
      checkFirstNameAliasConflict: function() {
          if (!this.inseeData || !this.inseeData.names.firstname) return false;

          const normalizedInsee = this.inseeData.names.firstname
              .toLowerCase()
              .normalize('NFD')
              .replace(/[\u0300-\u036f]/g, '');

          // Vérifier tous les champs de prénom alias (jusqu'à 20 pour être sûr)
          for (let i = 0; i < 20; i++) {
              const aliasField = document.getElementById(`first_name_alias${i}`);
              
              // Si le champ n'existe pas, on arrête la recherche
              if (!aliasField) break;
              
              // Si le champ est vide, on continue
              if (!aliasField.value) continue;

              // Normalisation et comparaison
              const normalizedAlias = aliasField.value
                  .toLowerCase()
                  .normalize('NFD')
                  .replace(/[\u0300-\u036f]/g, '');

              if (normalizedAlias === normalizedInsee) {
                  return i; // Retourne le numéro de l'alias en conflit
              }
          }

          return false; // Pas de conflit trouvé
      },

      processField: function(fieldType, onlyEmpty = false, event = null) {
          // Verify INSEE data exists first
          if (!this.inseeData) {
              console.warn('No INSEE data available');
              return false;
          }

          // Récupérer les valeurs selon le type de champ
          let value, targetId, isDate = false;

          // Vérifier le type de champ
          const fieldConfig = FIELD_IDS[fieldType];
          if (!fieldConfig) {
              console.warn('No field configuration for:', fieldType);
              return false;
          }

          // Récupérer les informations selon le type de champ
          if (fieldType === 'firstname') {
              // Check if names data exists
              if (!this.inseeData.names || !this.inseeData.names.firstname) {
                  console.log(`Missing data: firstname`);
                  return false;
              }

              // Vérifier si le prénom existe comme prénom alias
              const aliasConflict = this.checkFirstNameAliasConflict();
              if (aliasConflict !== false) {
                 this.addAliasWarningBanner(aliasConflict);
                 return true;
              }
              value = this.inseeData.names.firstname;
              targetId = 'first_name';
          }
          else if (fieldType === 'surname') {
              // Check if names data exists
              if (!this.inseeData.names || !this.inseeData.names.surname) {
                  console.log(`Missing data: surname`);
                  return false;
              }

              value = this.inseeData.names.surname;
              targetId = 'surname';
          }
          else if (fieldType.endsWith('-date')) {
              // Traitement spécial pour les dates
              isDate = true;
              const dateType = fieldType.split('-')[0]; // 'birth' ou 'death'

              // Check if date data exists
              if (!this.inseeData[dateType]) {
                  console.log(`Missing data: ${dateType}`);
                  return false;
              }

              const eventFields = this.findEventFields(dateType, true);

              if (!eventFields?.eventNum) {
                  console.warn('Could not find event number for', dateType);
                  return false;
              }

              // Appeler handleDateFields directement
              return this.handleDateFields(
                  `e_date${eventFields.eventNum}`,
                  this.inseeData[dateType],
                  fieldType,
                  onlyEmpty
              );
          }
          else if (fieldType === 'birth-place') {
              // Check if birth data exists
              if (!this.inseeData.birth || !this.inseeData.birth.place) {
                  console.log(`Missing data: birth place`);
                  return false;
              }

              value = this.inseeData.birth.place;
              const eventFields = this.findEventFields('birth', true);
              if (!eventFields?.eventNum) {
                  console.warn('Could not find event number for birth');
                  return false;
              }
              targetId = `e_place${eventFields.eventNum}`;
          }
          else if (fieldType === 'death-place') {
              // Check if death data exists
              if (!this.inseeData.death || !this.inseeData.death.place) {
                  console.log(`Missing data: death place`);
                  return false;
              }

              value = this.inseeData.death.place;
              const eventFields = this.findEventFields('death', true);
              if (!eventFields?.eventNum) {
                  console.warn('Could not find event number for death');
                  return false;
              }
              targetId = `e_place${eventFields.eventNum}`;
          }
          else if (fieldType === 'death-source') {
              // Check if source exists
              if (!this.inseeData.source) {
                  console.log(`Missing data: death source`);
                  return false;
              }

              value = this.inseeData.source;
              const eventFields = this.findEventFields('death', true);
              if (!eventFields?.eventNum) {
                  console.warn('Could not find event number for death source');
                  return false;
              }
              targetId = `e_src${eventFields.eventNum}`;
          }

          // Si on n'a pas pu déterminer l'ID cible ou la valeur
          if (!targetId || !value) {
              console.warn('Could not determine target ID or value for:', fieldType);
              return false;
          }

          // Récupérer l'élément
          const field = document.getElementById(targetId);
          if (!field) {
              console.warn('Field not found:', targetId);
              return false;
          }

          // Vérifier si on doit remplir ce champ
          if (onlyEmpty && field.value) {
              console.log(`Field ${targetId} not empty, skipping`);
              return false;
          }

          // Appliquer la valeur avec traitement spécial si nécessaire
          if (fieldType === 'death-source') {
              if (field.value && !field.value.includes('Insee')) {
                  field.value = field.value + ' ; ' + value;
              } else {
                  field.value = value;
              }
          } else {
              field.value = this.handleNameCase(value, targetId);

              // Donner le focus au champ prénom (fonctionnalité existante)
              if (fieldType === 'firstname') {
                  field.focus();
                  field.selectionStart = field.selectionEnd = field.value.length;
              }
          }

          // Mettre à jour l'indicateur visuel
          this.updateVisualFeedback(fieldType);

          return true;
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
                dd: document.querySelector(`input[name="${prefix}_dd"]`),
                mm: document.querySelector(`input[name="${prefix}_mm"]`),
                yyyy: document.querySelector(`input[name="${prefix}_yyyy"]`)
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
          // Cas spécial: prénom ignoré mais marqué comme validé
          if (type === 'firstname' && this.checkFirstNameAliasConflict()) {
              console.log('Prénom ignoré lors du remplissage automatique, marqué comme validé');
              this.updateVisualFeedback(type);
              return;
          }
          if (value && (!onlyEmpty || !document.getElementById(id)?.value)) {
              this.handleDataReplacement(id, value, type);
          }
      },

      // Ajoute un bandeau de warning si le prénom est déjà en prénom alias
      // aliasNumber: le numéro du champ alias en conflit (0, 1, 2, etc.)
      addAliasWarningBanner: function(aliasNumber) {
          if (!document.getElementById('alias-warning') && this.inseeData?.names?.firstname) {

              const warningDiv = document.createElement('div');
              warningDiv.id = 'alias-warning';
              warningDiv.className = 'alert alert-warning mt-2 mb-1';
              warningDiv.innerHTML = `<i class="fa fa-triangle-exclamation text-danger mr-1"></i>
                                     Prénom « ${this.inseeData.names.firstname} » ignoré car
                                     déjà renseigné comme prénom alias ${aliasNumber}.`;

              const inseeDataDiv = document.querySelector('.insee-data');
              if (inseeDataDiv) {
                  inseeDataDiv.after(warningDiv);
              }

              this.updateVisualFeedback('firstname');
          }
      },

      fillFields: function(onlyEmpty = false) {
          if (!this.inseeData) {
              console.warn('No INSEE data available');
              return;
          }

          this.setDeathStatus();

          // Traiter chaque type de champ avec la fonction centralisée
          const fieldTypes = [
              'firstname', 'surname',
              'birth-date', 'birth-place',
              'death-date', 'death-place', 'death-source'
          ];

          let fieldsUpdated = 0;

          fieldTypes.forEach(fieldType => {
              if (this.processField(fieldType, onlyEmpty, null)) {
                  fieldsUpdated++;
              }
          });

          console.log(`Updated ${fieldsUpdated} out of ${fieldTypes.length} fields`);

          this.checkAllValidated();
          return fieldsUpdated;
      },

      fillEmptyFields: function() {
          this.fillFields(true);
          this.updateButtonStates('empty');
      },

      fillAllFields: function() {
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
              // Si le champ est vide ou déjà en majuscules, on conserve les majuscules
              if (!currentValue || currentValue === currentValue.toUpperCase()) {
                  return value;
              }
              // Sinon on met en minuscules avec première lettre majuscule
              return value.toLowerCase().replace(/(?:^|\s)\S/g, c => c.toUpperCase());
          }

          // Pour les prénoms : toujours mettre en forme avec majuscules initiales
          if (fieldId === 'first_name') {
              // Convertit en minuscules puis met en majuscule après chaque espace ou tiret
              return value.toLowerCase().replace(/(?:^|\s|-)\S/g, c => c.toUpperCase());
          }

          // Pour tous les autres champs, aucun traitement
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

          // Cas spécial: ne pas remplacer le prénom si le même existe comme prénom alias
          // mais marquer tout de même la ligne prénom validée
          const aliasConflict = this.checkFirstNameAliasConflict();
          if (fieldType === 'firstname' && aliasConflict !== false) {
              console.log('Prénom ignoré car existe déjà comme prénom alias, marqué comme validé');
              this.updateVisualFeedback(fieldType);
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
              // Pour les dates on doit manipuler les trois champs séparément (jour, mois, année)
              const eventNum = targetId.match(/\d+/)?.[0];
              if (!eventNum) {
                  console.warn('Event number not found in targetId:', targetId);
                  return;
              }

              // Construire les IDs corrects pour les champs de date
              const basePrefix = `e_date${eventNum}`;
              const dateFields = {
                  dd: document.getElementById(`${basePrefix}_dd`),
                  mm: document.getElementById(`${basePrefix}_mm`),
                  yyyy: document.getElementById(`${basePrefix}_yyyy`)
              };

              // Vérifier que tous les champs existent
              if (!dateFields.dd || !dateFields.mm || !dateFields.yyyy) {
                  console.warn('Date fields not found:', dateFields);
                  return;
              }

              // Extraire les valeurs de date depuis l'objet INSEE
              const dateType = fieldType.startsWith('birth') ? 'birth' : 'death';
              const dateValues = {
                  dd: this.inseeData[dateType].day,
                  mm: this.inseeData[dateType].month,
                  yyyy: this.inseeData[dateType].year
              };

              // Remplir chaque champ individuellement
              let anyFieldUpdated = false;
              Object.entries(dateFields).forEach(([part, field]) => {
                  if (field && dateValues[part]) {
                      const newValue = dateValues[part];
                      if (field.value !== newValue) {
                          field.value = newValue;
                          anyFieldUpdated = true;
                      }
                  }
              });

              // Mettre à jour la rétroaction visuelle si au moins un champ a été mis à jour
              if (anyFieldUpdated) {
                  this.updateVisualFeedback(fieldType);
              }
          } else {
              console.log(`Capitalisation pour ${fieldType}:`, value, '→', this.handleNameCase(value, targetId));
              field.value = this.handleNameCase(value, targetId);
              this.updateVisualFeedback(fieldType);

              // Ajout du focus automatique pour le champ prénom
              if (fieldType === 'firstname') {
                  // Donner le focus au champ prénom
                  field.focus();
                  // Placer le curseur à la fin du texte
                  field.selectionStart = field.selectionEnd = field.value.length;
              }
          }
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

            window.setTimeout(() => this.checkAllValidated(), 50);
        }
    }
},

      // Setup click handlers for data replacement
      setupDataReplacementHandlers: function() {
    document.querySelectorAll('.replacement-value').forEach(span => {
        span.addEventListener('click', (e) => {
            const fieldType = e.target.dataset.fieldType;

            // Traiter le champ avec la fonction centralisée
            // Passer l'événement pour permettre l'affichage du tooltip
            this.processField(fieldType, false, e);
        });
    });
},

      checkAllValidated: function() {
          // Récupérer toutes les icônes de validation (sauf celle du titre)
          const allLines = document.querySelectorAll('.data-line .check-icon-container i');

          // Ne pas inclure la première ligne (titre) dans la vérification
          const fieldIcons = Array.from(allLines).slice(1);

          // Vérifier si toutes les icônes de champ ont la classe fa-check
          const allChecked = fieldIcons.every(icon =>
              icon.classList.contains('fa-check')
          );

          // Si tous les champs sont validés, mettre à jour l'icône du titre
          if (allChecked) {
              const titleIcon = document.querySelector('.data-line:first-child .check-icon-container i');
              if (titleIcon && !titleIcon.classList.contains('fa-check')) {
                  titleIcon.className = 'fa fa-check fa-xl text-success';
                  this.updateButtonStates('all');
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
          const birthPlaceBrut = data.birth.place_brut
              ? `<br><span class="user-select-all">${data.birth.place_brut}</span>`
              : '';
          const birthPlaceBrutTxt = data.birth.place_brut
          ? `<br><span class="user-select-none text-muted">  Lieu brut</span>` : '';
          const displayDiv = document.createElement('div');
          displayDiv.className = 'insee-blacklist bg-light p-3 mb-2 border rounded';

          const getResultSpan = (todoValue, resultValue, selectable = false) => {
              const isSame = todoValue === resultValue && todoValue !== '';
              const classes = [];

              // N'appliquer user-select-all que si les valeurs sont différentes ET selectable=true
              if (selectable) classes.push('user-select-all');
              classes.push(isSame ? 'text-muted' : 'text-success');

              const classAttr = classes.length ? `class="${classes.join(' ')}"` : '';
              return `<span ${classAttr}>${resultValue || ''}</span>`;
          };

          displayDiv.innerHTML = `
              <h4 class="mb-3">
                <span class="user-select-none">Données de référence </span>
                <span class="user-select-all">${data.blacklist.gwkey}</span>
                <span class="user-select-none"> – Score ${data.title.score}</span>
              </h4>

       <div class="d-flex flex-wrap text-monospace">
            <div class="d-flex align-self-center align-self-lg-start mr-2 mr-lg-4">
                <div class="pr-4">
                    <span class="text-muted">${data.title.todo_fn || ''}</span><br>
                    ${getResultSpan(data.title.todo_fn, data.title.result_fn, true)}
                </div>
                <div>
                    <span class="text-muted">${data.title.todo_sn || ''}</span><br>
                    ${getResultSpan(data.title.todo_sn, data.title.result_sn, true)}
                </div>
            </div>
            <div class="flex-grow-1 d-lg-flex">
              <div class="d-flex mb-2 mb-lg-0 mr-2 mr-lg-4">
                  <div class="pr-2">
                      <span class="text-muted">${data.title.todo_bd || ''}</span><br>
                      ${getResultSpan(data.title.todo_bd, data.title.result_bd, false)}
                      ${birthPlaceBrutTxt}
                  </div>
                  <div class="flex-grow-1">
                      <span class="text-muted">${data.title.todo_bp || ''}</span><br>
                      ${getResultSpan(data.title.todo_bp, data.title.result_bp, true)}
                      ${birthPlaceBrut}
                  </div>
              </div>
              <div class="d-flex">
                  <div class="pr-2">
                      <span class="text-muted">${data.title.todo_dd || ''}</span><br>
                      ${getResultSpan(data.title.todo_dd, data.title.result_dd, false)}
                  </div>
                  <div class="flex-grow-1">
                      <span class="text-muted">${data.title.todo_dp || ''}</span><br>
                      ${getResultSpan(data.title.todo_dp, data.title.result_dp, true)}
                  </div>
              </div>
          </div>
        </div>

            </div>
          `;

          return displayDiv;
      },

      createDataDisplay: function(data) {
          const rawdataDiv = this.createRawDataInfo(data);

          const displayDiv = document.createElement('div');
          displayDiv.className = 'insee-data bg-light p-3 my-2 border rounded';

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