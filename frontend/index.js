function ticketOnDragStart(event) {

    const ticket = event.target.id;
    const from = document.getElementById(ticket).columnId;

    event.dataTransfer.setData('ticket', ticket);
    event.dataTransfer.setData('from', from);
}

function phaseAllowDrop(event) {
    event.preventDefault();
}

function phaseDropTicket(event) {
    event.preventDefault();
    const fromId   = event.dataTransfer.getData('from')
    const toId     = event.target.id;  
    const ticketId = event.dataTransfer.getData('ticket')
    moveTicket(getCurrentBoard(), fromId, toId, ticketId);
}

function getCurrentBoard() {
  const queryString = window.location.search;
  const urlParams = new URLSearchParams(queryString);
  return urlParams.get('board');
}

function create(event) {
  const boardName  = getCurrentBoard();
  const strName    = document.getElementById('inp_name').value;
  const strContent = document.getElementById('inp_content').value;
  createTicket(boardName, strName, strContent);
}

function main() {
  const boardName = getCurrentBoard();
  fetchBoard(boardName);
}

main();