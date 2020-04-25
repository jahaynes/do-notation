
function ticketOnDragStart(event) {
    event.dataTransfer
         .setData('text/utf8', event.target.id);
}

function phaseAllowDrop(event) {
    event.preventDefault();
}

function phaseDropTicket(event) {
    event.preventDefault();

    const sourceId = event.dataTransfer.getData('text/utf8')
    const destId   = event.target.id;
    console.log("drop ticket: " + sourceId + " -> " + destId);

    const from = document.getElementById(sourceId).parentElement;
    const to   = document.getElementById(destId).parentElement;
    to.appendChild(from);
}

function columnHeaderAsElement(column) {

    ul = document.createElement('ul');
    ul.id = 'list_' + column.name;
    ul.classList.add('column');

    li = document.createElement('li');

    div = document.createElement('div');
    div.id = column.name;
    div.addEventListener('dragover', phaseAllowDrop);
    div.addEventListener('drop', phaseDropTicket);
    div.classList.add('phase');
    div.classList.add('noselect');
    div.textContent = column.name; // TODO pretty name

    li.appendChild(div);
    ul.appendChild(li);
    return ul;
}

function buildColumnHeaders(board) {
    for (const columnNo in board) {
        document
          .getElementById('header')
          .appendChild(columnHeaderAsElement(board[columnNo]));
    }
}

function ticketAsElement(jsonTicket) {

    li = document.createElement('li');

    div = document.createElement('div');
    div.id = jsonTicket.id;
    div.draggable = 'true';
    div.addEventListener('dragstart', ticketOnDragStart);
    div.classList.add('ticket');

    p = document.createElement('p');
    p.classList.add('noselect');
    p.textContent = jsonTicket.content;

    div.appendChild(p);
    li.appendChild(div);
    return li;
}

const fetchBoard = async () => {
  const response = await fetch('/board?board=some-board');
  const board = await response.json();

  buildColumnHeaders(board);
  fetchColumns(board);
}

const fetchColumns = async (board) => {
  for (const columnNo in board) {
    const response = await fetch('/column?columnId=' + board[columnNo].id);
    const column   = await response.json();
    for (const ticketNo in column) {
      document
        .getElementById('list_' + board[columnNo].name)
        .appendChild(ticketAsElement(column[ticketNo]));
    }
  }
}

const createTicket = async (name, content) => {

  const body = { 'board':   'some-board'
               , 'name':    name
               , 'content': content
               };

  const response = await fetch('/ticket/create', {
    method: 'POST',
    body: JSON.stringify(body),
    headers: {
      'Content-Type': 'application/json'
    }
  });

  console.log(response.status);
}

function create(event) {
 
  const strName    = document.getElementById('inp_name').value;
  const strContent = document.getElementById('inp_content').value;

  console.log(strName);
  console.log(strContent);

  createTicket(strName, strContent);
}

function main() {
  fetchBoard();
}

main();