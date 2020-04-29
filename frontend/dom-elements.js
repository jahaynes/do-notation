
function columnHeaderAsElement(column) {

    ul = document.createElement('ul');
    ul.id = 'list_' + column.name;
    ul.classList.add('column');

    li = document.createElement('li');

    div = document.createElement('div');
    div.id = column.id;
    div.addEventListener('dragover', phaseAllowDrop);
    div.addEventListener('drop', phaseDropTicket);
    div.classList.add('phase');
    div.classList.add('noselect');
    div.textContent = column.name; // TODO pretty name

    li.appendChild(div);
    ul.appendChild(li);
    return ul;
}

function ticketAsElement(columnId, jsonTicket) {

    li = document.createElement('li');
    li.classList.add('ticket-li');
    

    div = document.createElement('div');
    div.id = jsonTicket.id;
    div.columnId = columnId;
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

function buildColumnHeaders(board) {
    const tickets = document.getElementById('tickets');
    tickets.textContent = '';
    for (const columnNo in board) {
        tickets.appendChild(columnHeaderAsElement(board[columnNo]));
    }
}